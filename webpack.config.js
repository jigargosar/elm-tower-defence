const HtmlWebpackPlugin = require('html-webpack-plugin')

/*

npm i -D webpack webpack-cli webpack-dev-server html-webpack-plugin style-loader css-loader elm-webpack-loader

*/

module.exports = (_, config) => {
  const isProd = config.mode === 'production'
  return {
    entry: "./src/index2.js",
    output: {
      publicPath: '/',
    },
    resolve: {
      extensions: ['.js', '.elm'],
    },
    plugins: [new HtmlWebpackPlugin({ template: 'src/index.html' })],
    module: {
      rules: [
        {
          include: /\.elm/,
          use: [
            //'elm-hot-webpack-loader',
            {
              loader: 'elm-webpack-loader',
              options: { optimize: isProd, debug: !isProd },
            },
          ],
        },
        {
          include: /\.css/,
          use: [
            'style-loader',
            'css-loader'
            // POST CSS
            // { loader: 'css-loader', options: { importLoaders: 1 } },
            // {
            //   loader: 'postcss-loader',
            //   options: {
            //     ident: 'postcss',
            //     plugins: [require('tailwindcss')],
            //   },
            // },
          ],
        },
      ],
    },
    devServer: {
      historyApiFallback: true,
      hot: true,
      overlay: true,
    },
  }
}
