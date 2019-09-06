const CopyPlugin = require("copy-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const babelLoader = {
  loader: "babel-loader",
  options: { presets: ["@babel/preset-env"] }
};

const postCssLoader = mode => {
  const productionPlugins = [
    require("@fullhuman/postcss-purgecss")({
      content: ["./src/**/*.js", "./src/**/*.elm", "./src/**/*.html"],
      defaultExtractor: content => content.match(/[A-Za-z0-9-_:/]+/g) || []
    }),
    require("cssnano")
  ];

  return {
    loader: "postcss-loader",
    options: {
      plugins: [
        require("tailwindcss"),
        require("postcss-preset-env"),
        ...(mode === "production" ? productionPlugins : [])
      ]
    }
  };
};

const elmWebpackLoader = mode => ({
  loader: "elm-webpack-loader",
  options: {
    cwd: __dirname,
    runtimeOptions: "-A128m -H128m -n8m",
    debug: mode === "development",
    optimize: mode === "production"
  }
});

module.exports = (env, { mode }) => ({
  output: { filename: "[name].[contenthash].js" },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: babelLoader
      },
      {
        test: /\.css$/,
        use: [MiniCssExtractPlugin.loader, "css-loader", postCssLoader(mode)]
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: elmWebpackLoader(mode)
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({ template: "src/index.html" }),
    new MiniCssExtractPlugin({ filename: "[name].[contenthash].css" }),
    new CopyPlugin([{ from: "public" }])
  ]
});
