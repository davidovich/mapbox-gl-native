#pragma once

#include <memory>
#include <mbgl/style/expression/parsing_context.hpp>
#include <mbgl/style/expression/expression.hpp>
#include <mbgl/style/expression/definitions.hpp>
#include <mbgl/style/conversion.hpp>

namespace mbgl {
namespace style {
namespace expression {

using namespace mbgl::style;

template <class V>
std::string getJSType(const V& value) {
    using namespace mbgl::style::conversion;
    if (isUndefined(value)) {
        return "undefined";
    }
    if (isArray(value) || isObject(value)) {
        return "object";
    }
    optional<mbgl::Value> v = toValue(value);
    assert(v);
    return v->match(
        [&] (std::string) { return "string"; },
        [&] (bool) { return "boolean"; },
        [&] (auto) { return "number"; }
    );
}

using ParseResult = variant<CompileError, std::unique_ptr<UntypedExpression>>;

template <class V>
ParseResult parseExpression(const V& value, const ParsingContext& context)
{
    using namespace mbgl::style::conversion;
    
    if (isArray(value)) {
        const std::size_t length = arrayLength(value);
        if (length == 0) {
            CompileError error = {
                "Expected an array with at least one element. If you wanted a literal array, use [\"literal\", []].",
                context.key()
            };
            return error;
        }
        
        const optional<std::string>& op = toString(arrayMember(value, 0));
        if (!op) {
            CompileError error = {
                "Expression name must be a string, but found " + getJSType(arrayMember(value, 0)) +
                    " instead. If you wanted a literal array, use [\"literal\", [...]].",
                context.key(0)
            };
            return error;
        }
        
        if (*op == "literal") {
            if (length != 2) return CompileError {
                "'literal' expression requires exactly one argument, but found " + std::to_string(length - 1) + " instead.",
                context.key()
            };
            return UntypedLiteral::parse(arrayMember(value, 1), ParsingContext(context, {1}, {"literal"}));
        }

//        if (*op == "e") return MathConstant::e(context);
//        if (*op == "pi") return MathConstant::pi(context);
//        if (*op == "ln2") return MathConstant::ln2(context);
//        if (*op == "typeof") return LambdaExpression::parse<TypeOf>(value, context);
//        if (*op == "string") return LambdaExpression::parse<Assertion<std::string>>(value, context);
//        if (*op == "number") return LambdaExpression::parse<Assertion<float>>(value, context);
//        if (*op == "boolean") return LambdaExpression::parse<Assertion<bool>>(value, context);
//        if (*op == "array") return Array::parse(value, context);
//        if (*op == "to_string") return LambdaExpression::parse<ToString>(value, context);
//        if (*op == "to_number") return LambdaExpression::parse<ToNumber>(value, context);
//        if (*op == "to_boolean") return LambdaExpression::parse<ToBoolean>(value, context);
//        if (*op == "to_rgba") return LambdaExpression::parse<ToRGBA>(value, context);
//        if (*op == "parse_color") return LambdaExpression::parse<ParseColor>(value, context);
//        if (*op == "rgba") return LambdaExpression::parse<RGBA>(value, context);
//        if (*op == "rgb") return LambdaExpression::parse<RGB>(value, context);
//        if (*op == "get") return LambdaExpression::parse<Get>(value, context);
//        if (*op == "has") return LambdaExpression::parse<Has>(value, context);
//        if (*op == "at") return LambdaExpression::parse<At>(value, context);
//        if (*op == "length") return LambdaExpression::parse<Length>(value, context);
//        if (*op == "properties") return LambdaExpression::parse<Properties>(value, context);
//        if (*op == "id") return LambdaExpression::parse<Id>(value, context);
//        if (*op == "geometry_type") return LambdaExpression::parse<GeometryType>(value, context);
//        if (*op == "+") return LambdaExpression::parse<Plus>(value, context);
//        if (*op == "-") return LambdaExpression::parse<Minus>(value, context);
//        if (*op == "*") return LambdaExpression::parse<Times>(value, context);
//        if (*op == "/") return LambdaExpression::parse<Divide>(value, context);
//        if (*op == "^") return LambdaExpression::parse<Power>(value, context);
//        if (*op == "%") return LambdaExpression::parse<Mod>(value, context);
//        if (*op == "coalesce") return LambdaExpression::parse<Coalesce>(value, context);

        
        return CompileError {
            std::string("Unknown expression \"") + *op + "\". If you wanted a literal array, use [\"literal\", [...]].",
            context.key(0)
        };
    }
    
    if (isObject(value)) {
        return CompileError {
            "Bare objects invalid. Use [\"literal\", {...}] instead.",
            context.key()
        };
    }
    
    return UntypedLiteral::parse(value, context);
}


} // namespace expression
} // namespace style
} // namespace mbgl
