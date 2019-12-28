const sqs = require("@aws-cdk/aws-sqs");
const { SqsEventSource } = require("@aws-cdk/aws-lambda-event-sources");
const lambda = require("@aws-cdk/aws-lambda");
const cdk = require("@aws-cdk/core");

class NewsletterStack extends cdk.Stack {
  constructor(app, id) {
    super(app, id);

    const dlq = new sqs.Queue(this, "news-ids-dlq", {});

    const queue = new sqs.Queue(this, "news-ids-queue", {
      retentionPeriod: cdk.Duration.seconds(60),
      deadLetterQueue: {
        maxReceiveCount: 1,
        queue: dlq
      }
    });

    const haskellRuntime = lambda.LayerVersion.fromLayerVersionArn(
      this,
      "HaskellRuntime",
      "arn:aws:lambda:eu-west-1:785355572843:layer:aws-haskell-runtime:2"
    );

    const extractNewsLambda = new lambda.Function(this, "test", {
      code: lambda.Code.fromAsset("build/function.zip"),
      runtime: lambda.Runtime.PROVIDED,
      handler: "src/Lib.handler",
      timeout: cdk.Duration.seconds(10),
      layers: [haskellRuntime],
      environment: {"QUEUE_URL": queue.queueUrl}
    });

    queue.grantSendMessages(extractNewsLambda);
  }
}

module.exports = NewsletterStack;

const app = new cdk.App();
new NewsletterStack(app, "newsletter", {});
app.synth();
