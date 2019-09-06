const CopyPlugin = require("copy-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = (env, options) => ({
  output: { filename: "[name].[contenthash].js" },
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, use: "babel-loader" },
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          "css-loader",
          {
            loader: "postcss-loader",
            options: { config: { ctx: { mode: options.mode } } }
          }
        ]
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader",
        options: {
          cwd: __dirname,
          runtimeOptions: "-A128m -H128m -n8m",
          debug: options.mode === "development",
          optimize: options.mode === "production"
        }
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({ template: "src/index.html" }),
    new MiniCssExtractPlugin({ filename: "[name].[contenthash].css" }),
    new CopyPlugin([{ from: "public" }])
  ]
});
