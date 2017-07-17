#include <mbgl/style/expression/definitions.hpp>
#include <mbgl/tile/geometry_tile_data.hpp>

namespace mbgl {
namespace style {
namespace expression {

//
//Result<std::vector<Value>> evaluateArgs(const EvaluationParameters& params,
//                                        const LambdaExpression::Args& args)
//{
//    std::vector<Value> argValues;
//    for(const auto& arg : args) {
//        auto argValue = arg->evaluate(params);
//        if (!argValue) return argValue.error();
//        argValues.emplace_back(*argValue);
//    }
//    return argValues;
//}
//
//template<typename T, typename EvalFunc>
//EvaluationResult evaluateFromArgs(const EvaluationParameters& params,
//                                 const std::unique_ptr<Expression>& a0,
//                                 EvalFunc evaluate)
//{
//    const auto& a0value = a0->evaluate<T>(params);
//    if (!a0value) return a0value.error();
//    return evaluate(*a0value);
//}
//
//// TODO: get this working to replace all the overloads below
//
////template <typename ...Ts>
////struct restargs { using expression = std::unique_ptr<Expression>; };
////
////template <typename T, typename ...Ts, typename Eval>
////EvaluationResult evaluateFromArgs(const EvaluationParameters& params,
////                                 const std::unique_ptr<Expression>& a0,
////                                 const typename restargs<Ts...>::expression& args,
////                                 Eval evaluate)
////{
////    const auto& a0value = a0->evaluate<T>(params);
////    if (a0value.template is<EvaluationError>()) {
////        return a0value.template get<EvaluationError>();
////    }
////    return evaluateFromArgs<Ts...>(
////        params,
////        std::forward<const typename restargs<Ts...>::expression&>(args),
////        [&] (Ts... restValues) {
////            return evaluate(a0value.template get<T>(), std::forward<Ts...>(restValues)...);
////        }
////    );
////}
//
//template<typename T, typename U, typename EvalFunc>
//EvaluationResult evaluateFromArgs(const EvaluationParameters& params,
//                                 const std::unique_ptr<Expression>& a0,
//                                 const std::unique_ptr<Expression>& a1,
//                                 EvalFunc evaluate)
//{
//    const auto& a0value = a0->evaluate<T>(params);
//    if (!a0value) return a0value.error();
//    const auto& a1value = a1->evaluate<U>(params);
//    if (!a1value) return a1value.error();
//    return evaluate(*a0value, *a1value);
//}
//
//template<typename T0, typename T1, typename T2, typename EvalFunc>
//EvaluationResult evaluateFromArgs(const EvaluationParameters& params,
//                                 const std::unique_ptr<Expression>& a0,
//                                 const std::unique_ptr<Expression>& a1,
//                                 const std::unique_ptr<Expression>& a2,
//                                 EvalFunc evaluate)
//{
//    const auto& a0value = a0->evaluate<T0>(params);
//    if (!a0value) return a0value.error();
//    const auto& a1value = a1->evaluate<T1>(params);
//    if (!a1value) return a1value.error();
//    const auto& a2value = a2->evaluate<T2>(params);
//    if (!a2value) return a2value.error();
//    return evaluate(*a0value, *a1value, *a2value);
//}
//
//template<typename T0, typename T1, typename T2, typename T3, typename EvalFunc>
//EvaluationResult evaluateFromArgs(const EvaluationParameters& params,
//                                 const std::unique_ptr<Expression>& a0,
//                                 const std::unique_ptr<Expression>& a1,
//                                 const std::unique_ptr<Expression>& a2,
//                                 const std::unique_ptr<Expression>& a3,
//                                 EvalFunc evaluate)
//{
//    const auto& a0value = a0->evaluate<T0>(params);
//    if (!a0value) return a0value.error();
//    const auto& a1value = a1->evaluate<T1>(params);
//    if (!a1value) return a1value.error();
//    const auto& a2value = a2->evaluate<T2>(params);
//    if (!a2value) return a2value.error();
//    const auto& a3value = a3->evaluate<T3>(params);
//    if (!a3value) return a3value.error();
//    return evaluate(*a0value, *a1value, *a2value, *a3value);
//}
//
//
//EvaluationResult TypeOf::evaluate(const EvaluationParameters& params) const  {
//    const auto& result = args[0]->evaluate(params);
//    if (!result) return result.error();
//    return toString(typeOf(*result));
//}
//
//EvaluationResult Array::evaluate(const EvaluationParameters& params) const  {
//    const auto& result = args[0]->evaluate(params);
//    if (!result) { return result.error(); }
//    const Value& v = *result;
//    const auto& expected = getType().get<type::Array>();
//    const auto& actual = typeOf(v);
//    if (actual.is<type::Array>()) {
//        const auto& arrayType = actual.get<type::Array>();
//        bool match = (!expected.N || expected.N == arrayType.N);
//        if (expected.itemType.is<type::ValueType>()) {
//            match = match && (arrayType.itemType.is<type::StringType>() ||
//                arrayType.itemType.is<type::NumberType>() ||
//                arrayType.itemType.is<type::BooleanType>());
//        } else {
//            match = match && (toString(expected.itemType) == toString(arrayType.itemType));
//        }
//        
//        if (match) return EvaluationResult(v);
//    }
//    
//    return EvaluationError {
//        "Expected value to be of type " + toString(getType()) +
//        ", but found " + toString(actual) + " instead."
//    };
//}
//
//EvaluationResult Coalesce::evaluate(const EvaluationParameters& params) const {
//    for (auto it = args.begin(); it != args.end(); it++) {
//        const auto& arg = *it;
//        const auto& result = arg->evaluate(params);
//        if (std::next(it) == args.end()) return result;
//        if (!result || *result == Null) continue;
//        return *result;
//    }
//    return Null;
//}
//
//EvaluationResult ToString::evaluate(const EvaluationParameters& params) const {
//    const auto& result = args[0]->evaluate(params);
//    if (!result) return result.error();
//    return result->match(
//        [&] (const std::string& s) -> EvaluationResult { return s; },
//        [&] (float v) -> EvaluationResult { return stringify(v); },
//        [&] (bool v) -> EvaluationResult { return stringify(v); },
//        [&] (const NullValue& v) -> EvaluationResult { return stringify(v); },
//        [&] (const auto& v) -> EvaluationResult {
//            return EvaluationError {
//                "Expected a primitive value in [\"string\", ...], but found " + toString(typeOf(v)) + " instead."
//            };
//        }
//    );
//}
//
//EvaluationResult ToNumber::evaluate(const EvaluationParameters& params) const {
//    const auto& result = args[0]->evaluate(params);
//    if (!result) return result.error();
//    if (result->is<float>()) { return result->get<float>(); }
//    if (result->is<std::string>()) {
//        const std::string& s = result->get<std::string>();
//        try {
//            return std::stof(s);
//        } catch(std::exception) {
//        }
//    }
//    return EvaluationError {
//        "Could not convert " + stringify(*result) + " to number."
//    };
//}
//
//EvaluationResult ToBoolean::evaluate(const EvaluationParameters& params) const {
//    const auto& result = args[0]->evaluate(params);
//    if (!result) return result.error();
//    return result->match(
//        [&] (float f) { return (bool)f; },
//        [&] (const std::string& s) { return s.length() > 0; },
//        [&] (bool b) { return b; },
//        [&] (const NullValue&) { return false; },
//        [&] (const auto&) { return true; }
//    );
//}
//
//EvaluationResult ToRGBA::evaluate(const EvaluationParameters& params) const {
//    return evaluateFromArgs<mbgl::Color>(params, args[0], [&] (const mbgl::Color& color) {
//        return std::vector<Value> { color.r, color.g, color.b, color.a };
//    });
//}
//
//EvaluationResult ParseColor::evaluate(const EvaluationParameters& params) const {
//    return evaluateFromArgs<std::string>(params, args[0], [&] (const std::string& colorString) -> EvaluationResult {
//        const auto& result = mbgl::Color::parse(colorString);
//        if (result) return EvaluationResult(*result);
//        return EvaluationError {
//            "Could not parse color from value '" + colorString + "'"
//        };
//    });
//}
//
//EvaluationResult RGB::evaluate(const EvaluationParameters& params) const {
//    return evaluateFromArgs<float, float, float>(params, args[0], args[1], args[2],
//        [&] (float r, float g, float b) -> EvaluationResult {
//            return mbgl::Color(r / 255.0f, g / 255.0f, b / 255.0f, 1.0f);
//        }
//    );
//}
//
//EvaluationResult RGBA::evaluate(const EvaluationParameters& params) const {
//    return evaluateFromArgs<float, float, float, float>(params, args[0], args[1], args[2], args[3],
//        [&] (float r, float g, float b, float a) -> EvaluationResult {
//            return mbgl::Color(r / 255.0f, g / 255.0f, b / 255.0f, a);
//        }
//    );
//}
//
//bool Get::isFeatureConstant() const {
//    return args.size() == 1 ? false : LambdaExpression::isFeatureConstant();
//}
//EvaluationResult Get::evaluate(const EvaluationParameters& params) const {
//    if (args.size() == 1) {
//        return evaluateFromArgs<std::string>(params, args[0], [&] (const std::string& key) -> EvaluationResult {
//            const auto& value = params.feature.getValue(key);
//            if (!value) return EvaluationError { "Property '" + key + "' not found in feature.properties" };
//            return convertValue(*value);
//        });
//    } else {
//        return evaluateFromArgs<std::string, std::unordered_map<std::string, Value>>(
//            params,
//            args[0],
//            args[1],
//            [&] (const std::string& key, const std::unordered_map<std::string, Value>& object) -> EvaluationResult {
//                if (object.find(key) == object.end()) return EvaluationError { "Property '" + key + "' not found in object" };
//                return object.at(key);
//            }
//        );
//    }
//}
//
//bool Has::isFeatureConstant() const {
//    return args.size() == 1 ? false : LambdaExpression::isFeatureConstant();
//}
//EvaluationResult Has::evaluate(const EvaluationParameters& params) const {
//    if (args.size() == 1) {
//        return evaluateFromArgs<std::string>(params, args[0], [&] (const std::string& key) -> EvaluationResult {
//            const auto& value = params.feature.getValue(key);
//            return value ? true : false;
//        });
//    } else {
//        return evaluateFromArgs<std::string, std::unordered_map<std::string, Value>>(
//            params,
//            args[0],
//            args[1],
//            [&] (const std::string& key, const std::unordered_map<std::string, Value>& object) -> EvaluationResult {
//                return object.find(key) != object.end();
//            }
//        );
//    }
//}
//
//EvaluationResult At::evaluate(const EvaluationParameters& params) const {
//    return evaluateFromArgs<float, std::vector<Value>>(
//        params,
//        args[0],
//        args[1],
//        [&] (float index, const std::vector<Value>& arr) -> EvaluationResult {
//            const size_t i = index;
//            if (index > arr.size()) {
//                return EvaluationError {
//                    "Array index out of bounds: " + std::to_string(i) + " >= " + std::to_string(arr.size())
//                };
//            }
//            return arr[i];
//        }
//    );
//}
//
//EvaluationResult Length::evaluate(const EvaluationParameters& params) const {
//    const auto& result = args[0]->evaluate(params);
//    if (!result) return result.error();
//    return (float) result->match(
//        [&] (const std::string& s) { return s.size(); },
//        [&] (const std::vector<Value>& v) { return v.size(); },
//        [&] (const auto&) { assert(false); return -1; }
//    );
//}
//
//bool Properties::isFeatureConstant() const { return false; }
//EvaluationResult Properties::evaluate(const EvaluationParameters& params) const {
//    return convertValue(params.feature.getProperties());
//}
//
//bool Id::isFeatureConstant() const { return false; }
//EvaluationResult Id::evaluate(const EvaluationParameters& params) const {
//    const auto& id = params.feature.getID();
//    if (!id) return EvaluationError { "Property 'id' not found in feature" };
//    return id->match(
//        [&](const std::string& s) -> EvaluationResult { return s; },
//        [&](const auto& n) -> EvaluationResult { return *numericValue<float>(n); }
//    );
//}
//
//bool GeometryType::isFeatureConstant() const { return false; }
//EvaluationResult GeometryType::evaluate(const EvaluationParameters& params) const {
//    switch(params.feature.getType()) {
//        case FeatureType::Unknown: return std::string("Unknown");
//        case FeatureType::LineString: return std::string("LineString");
//        case FeatureType::Point: return std::string("Point");
//        case FeatureType::Polygon: return std::string("Polygon");
//    }
//}
//
//EvaluationResult Plus::evaluate(const EvaluationParameters& params) const  {
//    const auto& evaluated = evaluateArgs(params, args);
//    if (!evaluated) return evaluated.error();
//    float sum = 0.0f;
//    for (const Value& v : *evaluated) {
//        sum += v.get<float>();
//    }
//    return sum;
//}
//
//EvaluationResult Times::evaluate(const EvaluationParameters& params) const  {
//    const auto& evaluated = evaluateArgs(params, args);
//    if (!evaluated) return evaluated.error();
//    float product = 1.0f;
//    for (const Value& v : *evaluated) {
//        product *= v.get<float>();
//    }
//    return product;
//}
//
//EvaluationResult Minus::evaluate(const EvaluationParameters& params) const  {
//    return evaluateFromArgs<float, float>(params, args[0], args[1], [&] (float a, float b) {
//        return a - b;
//    });
//}
//
//EvaluationResult Divide::evaluate(const EvaluationParameters& params) const  {
//    return evaluateFromArgs<float, float>(params, args[0], args[1], [&] (float a, float b) {
//        return a / b;
//    });
//}
//
//EvaluationResult Mod::evaluate(const EvaluationParameters& params) const  {
//    return evaluateFromArgs<float, float>(params, args[0], args[1], [&] (float a, float b) {
//        return (float) fmod(a, b);
//    });
//}
//
//EvaluationResult Power::evaluate(const EvaluationParameters& params) const  {
//    return evaluateFromArgs<float, float>(params, args[0], args[1], [&] (float a, float b) {
//        return (float) pow(a, b);
//    });
//}


} // namespace expression
} // namespace style
} // namespace mbgl
