const purgecss = require("@fullhuman/postcss-purgecss")({
  content: ["./src/**/*.js", "./src/**/*.elm", "./src/**/*.html"],
  defaultExtractor: content => content.match(/[A-Za-z0-9-_:/]+/g) || []
});

module.exports = ({ options }) => ({
  plugins: [
    require("tailwindcss"),
    require("postcss-preset-env"),
    ...(options.mode === "production" ? [purgecss, require("cssnano")] : [])
  ]
});
