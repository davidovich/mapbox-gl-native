#pragma once

#include <array>
#include <vector>
#include <memory>
#include <mbgl/util/optional.hpp>
#include <mbgl/util/variant.hpp>
#include <mbgl/util/color.hpp>
#include <mbgl/style/expression/type.hpp>
#include <mbgl/style/expression/value.hpp>
#include <mbgl/style/expression/parsing_context.hpp>
#include <mbgl/style/conversion.hpp>


namespace mbgl {

class GeometryTileFeature;

namespace style {
namespace expression {

struct EvaluationError {
    std::string message;
};

struct EvaluationParameters {
    float zoom;
    const GeometryTileFeature& feature;
};

template<typename T>
class Result : private variant<EvaluationError, T> {
public:
    using variant<EvaluationError, T>::variant;
    using Value = T;
    
    explicit operator bool () const {
        return this->template is<T>();
    }
    
    // optional does some type trait magic for this one, so this might
    // be problematic as is.
    const T* operator->() const {
        assert(this->template is<T>());
        return std::addressof(this->template get<T>());
    }
    
    T* operator->() {
        assert(this->template is<T>());
        return std::addressof(this->template get<T>());
    }
    
    T& operator*() {
        assert(this->template is<T>());
        return this->template get<T>();
    }
    
    const T& operator*() const {
        assert(this->template is<T>());
        return this->template get<T>();
    }
    
    const EvaluationError& error() const {
        assert(this->template is<EvaluationError>());
        return this->template get<EvaluationError>();
    }
};

using EvaluationResult = Result<Value>;

struct CompileError {
    std::string message;
    std::string key;
};

class TypedExpression {
public:
    TypedExpression(type::Type type_) : type(type_) {}
    virtual ~TypedExpression() {};
    
    virtual bool isFeatureConstant() const { return true; }
    virtual bool isZoomConstant() const { return true; }
    virtual EvaluationResult evaluate(const EvaluationParameters& params) const = 0;
    
    /*
      Evaluate this expression to a particular value type T. (See expression/value.hpp for
      possible types T.)
    */
    template <typename T>
    Result<T> evaluate(const EvaluationParameters& params) {
        const auto& result = evaluate(params);
        if (!result) { return result.error(); }
        return result->match(
            [&] (const T& v) -> variant<EvaluationError, T> { return v; },
            [&] (const auto& v) -> variant<EvaluationError, T> {
                return EvaluationError {
                    "Expected value to be of type " + toString(valueTypeToExpressionType<T>()) +
                    ", but found " + toString(typeOf(v)) + " instead."
                };
            }
        );
    }
    
    EvaluationResult evaluate(float z, const Feature& feature) const;
    
    type::Type getType() const { return type; }
    
private:
    type::Type type;
};

using TypecheckResult = optional<std::unique_ptr<TypedExpression>>;

class UntypedExpression {
public:
    UntypedExpression(std::string key_) : key(key_) {}
    virtual ~UntypedExpression() {}
    
    std::string getKey() const { return key; }
    virtual TypecheckResult typecheck(const type::Type& expected, std::vector<CompileError>& errors) const = 0;
private:
    std::string key;
};

using ParseResult = variant<CompileError, std::unique_ptr<UntypedExpression>>;
template <class V>
ParseResult parseExpression(const V& value, const ParsingContext& context);

class TypedLiteral : public TypedExpression {
public:
    TypedLiteral(Value value_) : TypedExpression(typeOf(value_)) {}
    EvaluationResult evaluate(const EvaluationParameters&) const override {
        return value;
    }
private:
    Value value;
};

class UntypedLiteral : public UntypedExpression {
public:
    TypecheckResult typecheck(const type::Type& expected, std::vector<CompileError>& errors) const override {
        const auto& error = matchType(expected, typeOf(value));
        if (error) {
            errors.push_back({ *error, getKey() });
            return {};
        };
        return {std::make_unique<TypedLiteral>(value)};
    }

    template <class V>
    static ParseResult parse(const V& value, const ParsingContext& ctx) {
        const Value& parsedValue = parseValue(value);
        return std::make_unique<UntypedLiteral>(ctx.key(), parsedValue);
    }

private:
    UntypedLiteral(std::string key_, Value value_) : UntypedExpression(key_), value(value_) {}
    
    template <class V>
    static Value parseValue(const V& value) {
        using namespace mbgl::style::conversion;
        if (isUndefined(value)) return Null;
        if (isObject(value)) {
            std::unordered_map<std::string, Value> result;
            eachMember(value, [&] (const std::string& k, const V& v) -> optional<conversion::Error> {
                result.emplace(k, parseValue(v));
                return {};
            });
            return result;
        }
        if (isArray(value)) {
            std::vector<Value> result;
            const auto length = arrayLength(value);
            for(std::size_t i = 0; i < length; i++) {
                result.emplace_back(parseValue(arrayMember(value, i)));
            }
            return result;
        }
        
        optional<mbgl::Value> v = toValue(value);
        assert(v);
        return convertValue(*v);
    }
    
    Value value;
};

template <typename R, typename ...Params>
class TypedCompoundExpression : public TypedExpression {
public:
    using Args = std::array<std::unique_ptr<TypedExpression>, sizeof...(Params)>;
    
    TypedCompoundExpression(type::Type type,
                            R (*evaluate_)(Params...),
                            Args args_) :
        TypedExpression(type),
        evaluateFunction(evaluate_),
        args(std::move(args_))
    {}
    
    EvaluationResult evaluate(const EvaluationParameters& params) const override {
        return applyEvaluateFunction(params, std::index_sequence_for<Params...>{});
    }
    
private:
    template <std::size_t ...I>
    EvaluationResult applyEvaluateFunction(const EvaluationParameters& params, std::index_sequence<I...>) const
    {
        const std::vector<EvaluationResult>& evaluated = {std::get<I>(args)->evaluate(params)...};
        for (const auto& arg : evaluated) {
            if(!arg) return arg.error();
        }
        // TODO: assert correct runtime type of each arg value
        const R& result = evaluateFunction(evaluated.at(I)->get<Params>()...);
        if (!result) return result.error();
        return *result;
    }
    

    R (*evaluateFunction)(Params...);
    Args args;
};

struct SignatureBase {
    SignatureBase(type::Type result_, std::vector<type::Type> params_) :
        result(result_),
        params(params_)
    {}
    virtual ~SignatureBase() {}
    virtual std::unique_ptr<TypedExpression> makeTypedExpression(std::vector<std::unique_ptr<TypedExpression>>) const = 0;
    type::Type result;
    std::vector<type::Type> params;
};

template <typename R, typename ...Params>
struct Signature : SignatureBase {
    Signature(R (*evaluate_)(Params...)) :
        SignatureBase(
            valueTypeToExpressionType<typename R::Value>(),
            {valueTypeToExpressionType<Params>()...}
        ),
        evaluate(evaluate_)
    {}
    
    std::unique_ptr<TypedExpression> makeTypedExpression(std::vector<std::unique_ptr<TypedExpression>> args) const override {
        typename TypedCompoundExpression<R, Params...>::Args argsArray;
        std::copy_n(std::make_move_iterator(args.begin()), sizeof...(Params), argsArray.begin());
        return std::make_unique<TypedCompoundExpression<R, Params...>>(result,
                                                                       evaluate,
                                                                       std::move(argsArray));
    }
    
    R (*evaluate)(Params...);
};

struct CompoundExpression {
    using Definition = std::vector<std::unique_ptr<SignatureBase>>;
    
    static std::unordered_map<std::string, Definition> definitions;
    
//    TODO: haven't figured out how to instantiate Signature<> with each of the evalFunctions arguments

//    template <typename ...Evals, typename std::enable_if_t<sizeof...(Evals) != 0, int> = 0>
//    static int registerExpression(std::string name, Evals... evalFunctions) {
//        Definition definition = {std::make_unique<Signature>(evalFunctions)...};
//        const auto& t0 = definition.at(0)->result;
//        for (const auto& signature : definition) {
//            // TODO replace with real ==
//            assert(toString(t0) == toString(signature->result));
//        }
//        definitions.emplace(name, std::move(definition));
//        return 0;
//    }
    template <typename R, typename ...Params>
    static int registerSignature(std::string name, R (*evaluate) (Params...)) {
        auto signature = std::make_unique<Signature<R, Params...>>(evaluate);
        if (definitions.find(name) == definitions.end()) {
            definitions.emplace(name, std::vector<std::unique_ptr<SignatureBase>>());
        }
        definitions.at(name).push_back(std::move(signature));
        return 0;
    }

};

class UntypedCompoundExpression : public UntypedExpression {
public:
    using Args = std::vector<std::unique_ptr<UntypedExpression>>;
    
    template <class V>
    static ParseResult parse(const V& value, const ParsingContext& ctx) {
        using namespace mbgl::style::conversion;
        assert(isArray(value) && arrayLength(value) > 0);
        const auto& name = toString(arrayMember(value, 0));
        assert(name);
        
        if (CompoundExpression::definitions.find(*name) == CompoundExpression::definitions.end()) {
            return CompileError {
                std::string("Unknown expression \"") + *name + "\". If you wanted a literal array, use [\"literal\", [...]].",
                ctx.key(0)
            };
        }
        
        std::vector<std::unique_ptr<UntypedExpression>> args;
        auto length = arrayLength(value);
        for (std::size_t i = 1; i < length; i++) {
            auto parsed = parseExpression(arrayMember(value, i), ParsingContext(ctx, {i}, name));
            if (parsed.template is<CompileError>()) {
                return parsed;
            }
            args.push_back(std::move(parsed.template get<std::unique_ptr<UntypedExpression>>()));
        }
        
        return std::make_unique<UntypedCompoundExpression>(ctx.key(), *name, std::move(args));
    }

    TypecheckResult typecheck(const type::Type& expected, std::vector<CompileError>& errors) const override {
        const auto& definition = CompoundExpression::definitions.at(name);
        
        std::vector<CompileError> currentSignatureErrors;
        for (const auto& signature : definition) {
            currentSignatureErrors.clear();
            
            if (signature->params.size() != args.size()) {
                currentSignatureErrors.emplace_back(CompileError {
                    "Expected " + std::to_string(signature->params.size()) +
                    " arguments, but found " + std::to_string(args.size()) + " instead.",
                    getKey()
                });
                continue;
            }
            
            const auto& error = matchType(expected, signature->result);
            if (error) {
                currentSignatureErrors.emplace_back(CompileError {
                    *error,
                    getKey()
                });
                continue;
            }

            std::vector<std::unique_ptr<TypedExpression>> checkedArgs;
            for (size_t i = 0; i < args.size(); i++) {
                const auto& param = signature->params.at(i);
                const auto& arg = args.at(i);
                auto checked = arg->typecheck(param, currentSignatureErrors);
                if (checked) {
                    checkedArgs.push_back(std::move(*checked));
                }
            }
            
            if (currentSignatureErrors.size() == 0) {
                return signature->makeTypedExpression(std::move(checkedArgs));
            }
        }
        
        errors.insert(errors.end(), currentSignatureErrors.begin(), currentSignatureErrors.end());
        return {};
    }
    
private:
    UntypedCompoundExpression(std::string key, std::string name_, std::vector<std::unique_ptr<UntypedExpression>> args_) :
        UntypedExpression(key),
        name(name_),
        args(std::move(args_))
    {}
    std::string name;
    std::vector<std::unique_ptr<UntypedExpression>> args;
};


} // namespace expression
} // namespace style
} // namespace mbgl
