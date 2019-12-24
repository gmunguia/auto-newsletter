const lambda = require("@aws-cdk/aws-lambda");
const cdk = require("@aws-cdk/core");

class NewsletterStack extends cdk.Stack {
  constructor(app, id) {
    super(app, id);

    const haskellRuntime = lambda.LayerVersion.fromLayerVersionArn(
      this,
      "HaskellRuntime",
      "arn:aws:lambda:eu-west-1:785355572843:layer:aws-haskell-runtime:2"
    );

    new lambda.Function(this, "test", {
      code: lambda.Code.fromAsset("build/function.zip"),
      runtime: lambda.Runtime.PROVIDED,
      handler: "src/Lib.handler",
      layers: [haskellRuntime]
    });
  }
}

module.exports = NewsletterStack;

const app = new cdk.App();
new NewsletterStack(app, "newsletter", {});
app.synth();
