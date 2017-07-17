#include <mbgl/style/expression/expression.hpp>
#include <mbgl/tile/geometry_tile_data.hpp>

namespace mbgl {
namespace style {
namespace expression {

class GeoJSONFeature : public GeometryTileFeature {
public:
    const Feature& feature;

    GeoJSONFeature(const Feature& feature_) : feature(feature_) {}

    FeatureType getType() const override  {
        return apply_visitor(ToFeatureType(), feature.geometry);
    }
    PropertyMap getProperties() const override { return feature.properties; }
    optional<FeatureIdentifier> getID() const override { return feature.id; }
    GeometryCollection getGeometries() const override { return {}; }
    optional<mbgl::Value> getValue(const std::string& key) const override {
        auto it = feature.properties.find(key);
        if (it != feature.properties.end()) {
            return optional<mbgl::Value>(it->second);
        }
        return optional<mbgl::Value>();
    }
};


EvaluationResult TypedExpression::evaluate(float z, const Feature& feature) const {
    std::unique_ptr<const GeometryTileFeature> f = std::make_unique<const GeoJSONFeature>(feature);
    return this->evaluate(EvaluationParameters {z, *f});
}

// Compound expressions

std::unordered_map<std::string, CompoundExpression::Definition> CompoundExpression::definitions;

Result<bool> equalf(float a, float b) {
    return a == b;
}
Result<bool> equals(std::string a, std::string b) {
    return a == b;
}




} // namespace expression
} // namespace style
} // namespace mbgl
