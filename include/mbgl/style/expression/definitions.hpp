#include <mbgl/style/expression/expression.hpp>

namespace mbgl {
namespace style {
namespace expression {

//
//// Concrete expression definitions
//class MathConstant : public LambdaExpression {
//public:
//    MathConstant(const std::string& key, const std::string& name, float value_) :
//        LambdaExpression(key, name, {}, type::Number, {{}}),
//        value(value_)
//    {}
//    
//    EvaluationResult evaluate(const EvaluationParameters&) const override { return value; }
//    
//    std::unique_ptr<Expression> applyInferredType(const type::Type&, Args) const override {
//        return std::make_unique<MathConstant>(getKey(), getName(), value);
//    }
//    
//    // TODO: declaring these constants like `static constexpr double E = 2.718...` caused
//    // a puzzling link error.
//    static std::unique_ptr<Expression> ln2(const ParsingContext& ctx) {
//        return std::make_unique<MathConstant>(ctx.key(), "ln2", 0.693147180559945309417);
//    }
//    static std::unique_ptr<Expression> e(const ParsingContext& ctx) {
//        return std::make_unique<MathConstant>(ctx.key(), "e", 2.71828182845904523536);
//    }
//    static std::unique_ptr<Expression> pi(const ParsingContext& ctx) {
//        return std::make_unique<MathConstant>(ctx.key(), "pi", 3.14159265358979323846);
//    }
//private:
//    float value;
//};
//
//class TypeOf : public LambdaBase<TypeOf> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::StringType type() { return type::String; };
//    static std::vector<Params> signatures() {
//        return {{type::Value}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//// TODO: This doesn't work with CRTP. With:
//// `template <typename T> class Assertion : public LambdaBase<Assertion<T>>`,
//// there's an error referring to member 'args'.
//template <typename T>
//class Assertion : public LambdaExpression {
//public:
//    Assertion(const std::string& key, const std::string& name, Args args) :
//        LambdaExpression(key, name, std::move(args), Assertion::type(), Assertion::signatures())
//    {}
//    Assertion(const std::string& key, const std::string& name, const type::Type& type, Args args) :
//        LambdaExpression(key, name, std::move(args), type, Assertion::signatures())
//    {}
//
//    static type::Type type() { return valueTypeToExpressionType<T>(); }
//    static std::vector<LambdaExpression::Params> signatures() { return {{type::Value}}; };
//
//    std::unique_ptr<Expression> applyInferredType(const type::Type& type, Args args) const override {
//        return std::make_unique<Assertion>(getKey(), getName(), type, std::move(args));
//    }
//    
//    EvaluationResult evaluate(const EvaluationParameters& params) const override {
//        const auto& result = args[0]->template evaluate<T>(params);
//        if (result) return *result;
//        return result.error();
//    };
//};
//
//class Array : public LambdaBase<Array> {
//public:
//    Array(const std::string& key, const std::string& name, type::Type type, Args args) :
//        LambdaBase(key, name, type, std::move(args))
//    {}
//    
//    static std::vector<Params> signatures() { return {{type::Value}}; }
//    
//    template <typename V>
//    static ParseResult parse(const V& value, const ParsingContext& ctx) {
//        assert(isArray(value));
//        auto length = arrayLength(value);
//        if (length < 2) return CompileError { "Expected at least one argument to \"array\"", ctx.key() };
//        if (length > 4) return CompileError {
//            "Expected one, two, or three arguments to \"array\", but found " + std::to_string(length - 1) + " instead.",
//            ctx.key()
//        };
//        
//        const std::string& name = *toString(arrayMember(value, 0));
//        
//        auto arg = parseExpression(arrayMember(value, 1), ParsingContext(ctx, {1}, {"array"}));
//        if (arg.template is<CompileError>()) return arg.template get<CompileError>();
//        Args args;
//        args.push_back(std::move(arg.template get<std::unique_ptr<Expression>>()));
//        
//        // parse the optional item type and length arguments
//        optional<type::Type> itemType;
//        optional<size_t> N;
//        if (length > 2) {
//            const auto& itemTypeName = toString(arrayMember(value, 2));
//            if (itemTypeName && *itemTypeName == "string") {
//                itemType = {type::String};
//            } else if (itemTypeName && *itemTypeName == "number") {
//                itemType = {type::String};
//            } else if (itemTypeName && *itemTypeName == "boolean") {
//                itemType = {type::String};
//            } else {
//                return CompileError {
//                    "The item type argument to \"array\" must be one of ${Object.keys(types).join(', ')}",
//                    ctx.key(2)
//                };
//            }
//        }
//        if (length > 3) {
//            const auto& arrayLength = toNumber(arrayMember(value, 3));
//            if (!arrayLength) return CompileError {
//                "The length argument to \"array\" must be a number literal.",
//                ctx.key(3)
//            };
//            N = static_cast<size_t>(*arrayLength);
//        }
//        return std::make_unique<Array>(ctx.key(), name, type::Array(type::Value), std::move(args));
//    }
//    
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Coalesce : public LambdaBase<Coalesce> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Typename("T"); };
//    static std::vector<Params> signatures() { return {{ NArgs { {type::Typename("T")}, {} } }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Match : public LambdaBase<Match> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Typename("T"); };
//    static std::vector<Params> signatures() {
//        return {{
//            type::Typename("T"),
//            NArgs { {type::Array(type::Typename("U")), type::Typename("T")}, {} },
//            type::Typename("T")
//        }};
//    };
//    
//    template <typename V>
//    static ParseResult parse(const V& value, const ParsingContext& ctx) {
//        assert(isArray(value));
//        auto length = arrayLength(value);
//        if (length < 3) {
//            return CompileError {
//                "Expected at least 2 arguments, but found only " + std::to_string(length - 1) + ".",
//                ctx.key()
//            };
//        }
//        
//        Args args;
//        
//        for (size_t i = 2; i < length; i++) {
//            const auto& arg = arrayMember(value, i);
//            if (i % 2 == 0) {
//                // Match inputs are provided as either a literal value or a
//                // raw JSON array of string / number / boolean values.
//                if (isArray(arg)) {
//                    auto groupLength = arrayLength(arg);
//                    if (groupLength == 0) return CompileError {
//                        "Expected at least one input value",
//                        ctx.key(i)
//                    };
//                    for (size_t j = 0; j < groupLength; j++) {
//                        const auto& input = arrayMember(arg, j);
//                        if (isArray(input) || isObject(input)) return CompileError {
//                            "Match inputs must be literal primitive values or arrays of literal primitive values.",
//                            ctx.key(i)
//                        };
//                    }
//                }
//            } else {
//                const auto& parsed = parseExpression(arg, ParsingContext(ctx, {i}, {"match"}));
//                if (parsed.template is<CompileError>()) {
//                    return parsed.template get<CompileError>();
//                }
//                args.push_back(std::move(parsed.template get<std::unique_ptr<Expression>>()));
//            }
//        }
//    }
//    
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Curve : public LambdaBase<Curve> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Typename("T"); };
//    static std::vector<Params> signatures() {
//        return {{
//            type::Number,
//            NArgs { {type::Number, type::Typename("T")}, {} }
//        }};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class ToString : public LambdaBase<ToString> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::String; };
//    static std::vector<Params> signatures() { return {{ type::Value }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class ToNumber : public LambdaBase<ToNumber> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Number; };
//    static std::vector<Params> signatures() { return {{ type::Value }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class ToBoolean : public LambdaBase<ToBoolean> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Boolean; };
//    static std::vector<Params> signatures() { return {{ type::Value }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class ToRGBA : public LambdaBase<ToRGBA> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Array(type::Number, 4); };
//    static std::vector<Params> signatures() { return {{ type::Color }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class ParseColor : public LambdaBase<ParseColor> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Color; };
//    static std::vector<Params> signatures() { return {{ type::String }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class RGB : public LambdaBase<RGB> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Color; };
//    static std::vector<Params> signatures() { return {{ type::Number, type::Number, type::Number }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class RGBA : public LambdaBase<RGBA> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Color; };
//    static std::vector<Params> signatures() { return {{ type::Number, type::Number, type::Number, type::Number }}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Get : public LambdaBase<Get> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::ValueType type() { return type::Value; };
//    static std::vector<Params> signatures() {
//        return {{type::String, NArgs { {type::Object}, 1 }}};
//    };
//    bool isFeatureConstant() const override;
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Has : public LambdaBase<Has> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Boolean; };
//    static std::vector<Params> signatures() {
//        return {{type::String, NArgs { {type::Object}, 1 }}};
//    };
//    bool isFeatureConstant() const override;
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class At : public LambdaBase<At> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Typename("T"); };
//    static std::vector<Params> signatures() {
//        return {{type::Number, type::Array(type::Typename("T"))}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Length : public LambdaBase<Length> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {
//            {type::Array(type::Typename("T"))},
//            {type::String}
//        };
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Properties : public LambdaBase<Properties> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Object; };
//    static std::vector<Params> signatures() { return {{}}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//    bool isFeatureConstant() const override;
//};
//
//class Id : public LambdaBase<Id> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::Value; };
//    static std::vector<Params> signatures() { return {{}}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//    bool isFeatureConstant() const override;
//};
//
//class GeometryType : public LambdaBase<GeometryType> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::Type type() { return type::String; };
//    static std::vector<Params> signatures() { return {{}}; };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//    bool isFeatureConstant() const override;
//};
//
//class Plus : public LambdaBase<Plus> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::NumberType type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {{NArgs {{type::Number}, {}}}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Times : public LambdaBase<Times> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::NumberType type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {{NArgs {{type::Number}, {}}}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Minus : public LambdaBase<Minus> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::NumberType type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {{type::Number, type::Number}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Divide : public LambdaBase<Divide> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::NumberType type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {{type::Number, type::Number}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Mod : public LambdaBase<Mod> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::NumberType type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {{type::Number, type::Number}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};
//
//class Power : public LambdaBase<Power> {
//public:
//    using LambdaBase::LambdaBase;
//    static type::NumberType type() { return type::Number; };
//    static std::vector<Params> signatures() {
//        return {{type::Number, type::Number}};
//    };
//    EvaluationResult evaluate(const EvaluationParameters& params) const override;
//};


} // namespace expression
} // namespace style
} // namespace mbgl
