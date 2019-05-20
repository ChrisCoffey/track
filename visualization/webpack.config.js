const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  entry: './src/viz.ts'
  ,devtool: 'inline-source-map'
  ,module: {
    rules: [
      {
        test: /\.tsx?$/
        ,use: 'ts-loader'
        ,exclude: /node_modules/
      }
    ]
  }
  ,resolve: {
    extensions: [ '.tsx', '.ts', '.js' ]
  }
  ,output: {
    filename: 'bundle.js'
    ,path: path.resolve(__dirname, 'dist')
  }
  ,devServer: {
    contentBase: path.join(__dirname, 'dist'),
    compress: true,
    port: 9000,
    hot: true
  }
  ,plugins: [
    new HtmlWebpackPlugin({
       title: 'My Time'
      ,template: 'index.html'
    })
  ]
};