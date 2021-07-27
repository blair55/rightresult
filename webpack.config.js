// Template for webpack.config.js in Fable projects
// Find latest version in https://github.com/fable-compiler/webpack-config-template

// In most cases, you'll only need to edit the CONFIG object (after dependencies)
// See below if you need better fine-tuning of Webpack options

var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var CopyWebpackPlugin = require("copy-webpack-plugin");
var MiniCssExtractPlugin = require("mini-css-extract-plugin");
var WebpackPwaManifest = require("webpack-pwa-manifest");
var WorkboxPlugin = require("workbox-webpack-plugin");

var CONFIG = {
  // The tags to include the generated JS and CSS will be automatically injected in the HTML template
  // See https://github.com/jantimon/html-webpack-plugin
  indexHtmlTemplate: "./src/Client/index.html",
  serviceWorkerTemplate: "./src/Client/sw.js",
  fsharpEntry: './src/Client/Client.fs.js',
  cssEntry: "./src/Client/style.sass",
  badgeCssEntry: "./src/Client/badges.css",
  outputDir: "./deploy/public",
  assetsDir: "./src/Client/public",
  logoPath: "./src/Client/public/logo.png",
  devServerPort: 8080,
  // When using webpack-dev-server, you may need to redirect some calls
  // to a external API server. See https://webpack.js.org/configuration/dev-server/#devserver-proxy
  devServerProxy: {
    // redirect requests that start with /api/* to the server on port 8085
    "/api/**": {
      target: "http://localhost:" + (process.env.SERVER_PROXY_PORT || "8085"),
      changeOrigin: true,
    },
    // redirect websocket requests that start with /socket/* to the server on the port 8085
    "/socket/**": {
      target: "http://localhost:" + (process.env.SERVER_PROXY_PORT || "8085"),
      ws: true,
    },
  }
};

// If we're running the webpack-dev-server, assume we're in development mode
var isProduction = !process.argv.find((v) => v.indexOf("webpack-dev-server") !== -1);
var environment = isProduction ? 'production' : 'development';
process.env.NODE_ENV = environment;
console.log('Bundling for ' + environment + '...');

// The HtmlWebpackPlugin allows us to use a template for the index.html page
// and automatically injects <script> or <link> tags for generated bundles.
var commonPlugins = [
  new webpack.ProgressPlugin(),
  new HtmlWebpackPlugin({
    filename: "index.html",
    template: resolve(CONFIG.indexHtmlTemplate),
  }),
  new WebpackPwaManifest({
    name: "Right Result",
    background_color: "#00d1b2",
    theme_color: "#00d1b2",
    orientation: "omit",
    ios: true,
    icons: [
      {
        src: resolve(CONFIG.logoPath),
        sizes: [96, 128, 192, 256, 384, 512],
        destination: path.join("icons", "android"),
      },
      {
        src: resolve(CONFIG.logoPath),
        sizes: [120, 152, 167, 180, 512],
        destination: path.join("icons", "ios"),
        ios: true,
      },
      {
        src: resolve(CONFIG.logoPath),
        size: 512,
        destination: path.join("icons", "ios"),
        ios: "startup",
      },
    ],
  }),
];

var modePlugins = isProduction
  ? [
    new MiniCssExtractPlugin({ filename: "style.[name].[hash].css" }),
    new CopyWebpackPlugin({ patterns: [{ from: resolve(CONFIG.assetsDir) }]}),
  ]
  : [new webpack.HotModuleReplacementPlugin()];

var commonPluginsLast = new WorkboxPlugin.InjectManifest({
  swSrc: resolve(CONFIG.serviceWorkerTemplate),
  swDest: "serviceworker.js",
});

module.exports = {
  // In development, bundle styles together with the code so they can also
  // trigger hot reloads. In production, put them in a separate CSS file.
  stats: false,
  entry: isProduction
    ? {
      app: [
        resolve(CONFIG.fsharpEntry),
        resolve(CONFIG.cssEntry),
        resolve(CONFIG.badgeCssEntry),
      ],
    }
    : {
      app: [resolve(CONFIG.fsharpEntry)],
      style: [resolve(CONFIG.cssEntry), resolve(CONFIG.badgeCssEntry)],
    },
  // Add a hash to the output file name in production
  // to prevent browser caching if code changes
  output: {
    publicPath: "/",
    path: resolve(CONFIG.outputDir),
    filename: isProduction ? "[name].[hash].js" : "[name].js",
  },
  mode: isProduction ? "production" : "development",
  devtool: isProduction ? "source-map" : "eval-source-map",
  optimization: {
    splitChunks: {
      chunks: "all",
    },
  },
  // Besides the HtmlPlugin, we use the following plugins:
  // PRODUCTION
  //      - MiniCssExtractPlugin: Extracts CSS from bundle to a different file
  //          To minify CSS, see https://github.com/webpack-contrib/mini-css-extract-plugin#minimizing-for-production
  //      - CopyWebpackPlugin: Copies static assets to output directory
  // DEVELOPMENT
  //      - HotModuleReplacementPlugin: Enables hot reloading when code changes without refreshing
  plugins: commonPlugins.concat(modePlugins, commonPluginsLast),
  resolve: {
    // See https://github.com/fable-compiler/Fable/issues/1490
    symlinks: false,
  },
  // Configuration for webpack-dev-server
  devServer: {
    publicPath: "/",
    contentBase: resolve(CONFIG.assetsDir),
    host: '0.0.0.0',
    port: CONFIG.devServerPort,
    proxy: CONFIG.devServerProxy,
    historyApiFallback: true,
    hot: true,
    inline: true,
    stats: false
  },
  // - sass-loaders: transforms SASS/SCSS into JS
  // - file-loader: Moves files referenced in the code (fonts, images) into output folder
  module: {
    rules: [
      {
        test: /\.(sass|scss|css)$/,
        use: [
          isProduction ? MiniCssExtractPlugin.loader : "style-loader",
          "css-loader",
          {
            loader: "sass-loader",
            options: { implementation: require("sass") },
          },
        ],
      },
      {
        test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
        use: ["file-loader"],
      },
    ],
  },
  resolve: {
    alias: {
      '/images': resolve(CONFIG.assetsDir + "/images"),
    },
  },
};

function resolve(filePath) {
  return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}
