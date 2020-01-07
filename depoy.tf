provider "aws" {
  region                  = "ap-southeast-2"
  profile                 = "jky-all"
}

resource "aws_lambda_function" "lambda_function" {
  function_name = "haskell-works-hal-example"

  s3_bucket = "dl.john-ky.io"
  s3_key = "haskell-works/lambdas/hal-example/v20/hal-example.zip"

  role = "${aws_iam_role.lambda_iam.arn}"

  handler = "provided"
  runtime = "provided"

  memory_size = 2048
  timeout = "300"
}

resource "aws_iam_role" "lambda_iam" {
  name = "haskell-works-hal-example"

  force_detach_policies = true

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}


resource "aws_lambda_event_source_mapping" "lambda-sqs-events" {
  batch_size = 1
  event_source_arn = "${aws_sqs_queue.source_queue.arn}"
  enabled = true
  function_name = "${aws_lambda_function.lambda_function.arn}"
}

resource "aws_sqs_queue" "source_queue" {
  name = "haskell-works-hal-example-source"
  visibility_timeout_seconds = 330 # 5.5 minutes
  message_retention_seconds = 1209600
}

resource "aws_sqs_queue" "source_target" {
  name = "haskell-works-hal-example-source"
  visibility_timeout_seconds = 330 # 5.5 minutes
  message_retention_seconds = 1209600
}
