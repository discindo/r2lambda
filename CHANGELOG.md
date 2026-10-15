## 0.1.0 (2026-10-12)


### New features

* accept folders in support_path ([27254c3](https://github.com/discindo/r2lambda/commit/27254c3826833cecaff6014db161ae592735fb87))
* add renv logic when creating the dockerfile ([5e94146](https://github.com/discindo/r2lambda/commit/5e94146af07eb8f15226e4bb8b5a2a1d726ceb4d))
* allow support files (multi-file project) ([8180779](https://github.com/discindo/r2lambda/commit/8180779cd3fed5e8205fcd83c13794844969e959))
* allow support files (multi-file project) ([95f9949](https://github.com/discindo/r2lambda/commit/95f994989c4f19ecfe3e5b4dff077b67d45155fe))
* build and deploy arm64 Lambda functions ([d9b3bde](https://github.com/discindo/r2lambda/commit/d9b3bde3c807e410a74a0dad3d724e7cd9ac3bb3))
* check lambda state before invoking ([bf37034](https://github.com/discindo/r2lambda/commit/bf37034d91204f71a33887ba13d2d7c637d700f5))
* deploy with an existing IAM role ([8cfe969](https://github.com/discindo/r2lambda/commit/8cfe9695bcf2c8e547c3bd812480cc73fb6c6935)), closes [#6](https://github.com/discindo/r2lambda/issues/6)
* grant AWS access through the Lambda execution role ([1ec810a](https://github.com/discindo/r2lambda/commit/1ec810a20d20bd2fd4c6185dce2bcac229cd4b42))
* install system packages in the Lambda image ([3d642a6](https://github.com/discindo/r2lambda/commit/3d642a6f3f62db219fa0451e754e8ae89c223741)), closes [#5](https://github.com/discindo/r2lambda/issues/5)
* parse ecr token. ([9adf88a](https://github.com/discindo/r2lambda/commit/9adf88a18f1603e0378b6c674f345fe2e245f4bc))
* separate build from deploy ([e2847ca](https://github.com/discindo/r2lambda/commit/e2847cacf48b0a43652ae55cb7cb3ddd6aa7bc47))
* start, test, and stop a lambda container locally ([d252680](https://github.com/discindo/r2lambda/commit/d252680f69641b0c4073b0ca7ffea23133abf989))
* start, test, and stop a lambda container locally ([dc213d2](https://github.com/discindo/r2lambda/commit/dc213d2c2e8a517b6f934a61ec8dccee8e235eb8))
* try to catch failed invocation, including due to Inactive state ([881f329](https://github.com/discindo/r2lambda/commit/881f329d98b96ffb06a0304b98386554dfa99948))
* update lambda to run on schedule ([7f4472c](https://github.com/discindo/r2lambda/commit/7f4472c5ad6e5d8638d8892626ddfe1b068887b6))
* use magrittr pipe ([6fa8575](https://github.com/discindo/r2lambda/commit/6fa85759d5d60ee9f1d192e497eb8b1c75d81282))
* use stevedore for push to ecr ([3e6641a](https://github.com/discindo/r2lambda/commit/3e6641a1cdf5db235a1d35969833e77582d2eab3))
* use stevedore to create the docker image ([a12a491](https://github.com/discindo/r2lambda/commit/a12a491e987cb256e5e257f704f8dc096af9e83c))


### Bug fixes

* build Lambda images on Amazon Linux 2023 with R 4.6.1 ([d573317](https://github.com/discindo/r2lambda/commit/d573317b82e7903e799f1e69e0ec5dc033b81c65))
* call the docker CLI instead of stevedore ([c711055](https://github.com/discindo/r2lambda/commit/c711055a1919765df527e8ea55ff3920bc43d7ba)), closes [#11](https://github.com/discindo/r2lambda/issues/11)
* check the image architecture before pushing it ([d74aaf3](https://github.com/discindo/r2lambda/commit/d74aaf3d69ad494bc01c561890e17ed6a33cac3c))
* correct log msg ([79a3df0](https://github.com/discindo/r2lambda/commit/79a3df0cce63ea9f535102c8d701eceb7a6eac49))
* don't create an ECR repository when deploying ([a669cef](https://github.com/discindo/r2lambda/commit/a669cef5b6c0981b614e696c8eac6d8c5d743506))
* don't need to check for aws cli anymore ([673b69c](https://github.com/discindo/r2lambda/commit/673b69c7720182b3c6bf87d16a5c963d2fd2f741))
* inspect the same image that deploy_lambda pushes ([ec91125](https://github.com/discindo/r2lambda/commit/ec91125ca8dfe250f57c047c41735d40951c09c8))
* install from github in readme ([61e6435](https://github.com/discindo/r2lambda/commit/61e643567f470e9d556cc476c00ddddcfe900565))
* look up AWS resources by name instead of listing them ([a99bfc0](https://github.com/discindo/r2lambda/commit/a99bfc0b6b3b51e9fc63e3ff459001928ea5ee3b))
* name support files in the Dockerfile as they are copied ([bbf5296](https://github.com/discindo/r2lambda/commit/bbf5296ed6b1db31665fe243178d11b464acefc1))
* report AWS errors from invoke_lambda and deploy_lambda ([1ceef40](https://github.com/discindo/r2lambda/commit/1ceef40921336ac8998f6c431036bc8149ae93fc))
* tempdir issue in tests ([28032d9](https://github.com/discindo/r2lambda/commit/28032d9bc7613d2b5b11fdad4198a2f786f1aecd))


### Documentation

* add Novica Nakov as maintainer and ORCIDs for both authors ([9c20ff7](https://github.com/discindo/r2lambda/commit/9c20ff72ed42a271fa9c596199a10bc943e6f1ff))
* build ignore docs folder ([8a5af02](https://github.com/discindo/r2lambda/commit/8a5af02af3cd512f79815d8c15e1a64d8992bd7e))
* code of conduct. ([9608875](https://github.com/discindo/r2lambda/commit/96088754bc19dd9a7df75494b2cd4b730d0f4ed8))
* correction in readme ([55b077a](https://github.com/discindo/r2lambda/commit/55b077a32360e4fd2bc57df16edf61619492f80b))
* document role, policy, support folder, system package and architecture options ([e2a5d07](https://github.com/discindo/r2lambda/commit/e2a5d072de89ad2516ef33d310da2c23da0c52ac))
* fix moved blog link in lambda-s3 vignette ([4471463](https://github.com/discindo/r2lambda/commit/4471463b9fa13e383b036a86be03b1e7dc05bf18))
* manual files ([16ba1e2](https://github.com/discindo/r2lambda/commit/16ba1e261a38186613853031b5096d90864be44d))
* manual files ([4c16601](https://github.com/discindo/r2lambda/commit/4c16601c2b3a4c23447efa52d148401f577d682b))
* manual files and vignette ([f470ef7](https://github.com/discindo/r2lambda/commit/f470ef700cade46c0a918c81baf34f5e5e20d0fa))
* pkg description ([9b778d1](https://github.com/discindo/r2lambda/commit/9b778d1f880d4ccf29ac6823b63de0c3ebdb6625))
* typo ([41386a6](https://github.com/discindo/r2lambda/commit/41386a64fb6c76979dff7df16fdde333fd420882))
* update readme ([a002958](https://github.com/discindo/r2lambda/commit/a002958d5fff2ee1c339171ad3a48a9ce3d6d561))
* update readme ([f89c595](https://github.com/discindo/r2lambda/commit/f89c5957604c8a2ceec9334d959ca691d150add1))
