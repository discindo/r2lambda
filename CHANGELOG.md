## [0.1.0](https://github.com/discindo/r2lambda/compare/v0.1.0...v0.1.0) (2026-10-17)


### New features

* add delete_lambda() to remove a function's AWS resources ([#41](https://github.com/discindo/r2lambda/issues/41)) ([13abc85](https://github.com/discindo/r2lambda/commit/13abc855b020902f7f14a916fcd34ce72f7d40d8))


### Bug fixes

* send EventBridge target ID as a string ([#40](https://github.com/discindo/r2lambda/issues/40)) ([d69201a](https://github.com/discindo/r2lambda/commit/d69201a6c0f43d3e609556c16198e83e84e9cd81))


### Documentation

* convert vignettes to Quarto ([#37](https://github.com/discindo/r2lambda/issues/37)) ([2fd68c7](https://github.com/discindo/r2lambda/commit/2fd68c7d1e9506929859dd1596c6b0d48063952c))
* maintain README.md directly and drop README.Rmd ([#39](https://github.com/discindo/r2lambda/issues/39)) ([3b2ff98](https://github.com/discindo/r2lambda/commit/3b2ff98a52bc36b4f195308ea18a053a29876ccf))
* meet CRAN extra checks ([#42](https://github.com/discindo/r2lambda/issues/42)) ([4968dbf](https://github.com/discindo/r2lambda/commit/4968dbf65a62545b6cf2ff92a87207ac62df9630))

## 0.1.0 (2026-10-12)


### New features

* accept folders in support_path ([27254c3](https://github.com/discindo/r2lambda/commit/33725e644636c1a8be2c10793192dce5b6e53308))
* add renv logic when creating the dockerfile ([5e94146](https://github.com/discindo/r2lambda/commit/a7d42fff132825c7c4af16eebbcc05ab705969c2))
* allow support files (multi-file project) ([8180779](https://github.com/discindo/r2lambda/commit/5fd71e166887344d66ec9e5d6c19c38b8b49058a))
* allow support files (multi-file project) ([95f9949](https://github.com/discindo/r2lambda/commit/44379b375e7961912ead279293ebde343549df06))
* build and deploy arm64 Lambda functions ([d9b3bde](https://github.com/discindo/r2lambda/commit/c185b116a317dad6fbaa25e67e2aef642fb51c2b))
* check lambda state before invoking ([bf37034](https://github.com/discindo/r2lambda/commit/bf37034d91204f71a33887ba13d2d7c637d700f5))
* deploy with an existing IAM role ([8cfe969](https://github.com/discindo/r2lambda/commit/fc25d4d0b35e6969d87f97d5689caddfc729d0c1)), closes [#6](https://github.com/discindo/r2lambda/issues/6)
* grant AWS access through the Lambda execution role ([1ec810a](https://github.com/discindo/r2lambda/commit/0537753e829102e4e2b2b4581dbbc5855a608f6d))
* install system packages in the Lambda image ([3d642a6](https://github.com/discindo/r2lambda/commit/256e40f01938cda26531868436b4ebb46bfbe8b3)), closes [#5](https://github.com/discindo/r2lambda/issues/5)
* parse ecr token. ([9adf88a](https://github.com/discindo/r2lambda/commit/9adf88a18f1603e0378b6c674f345fe2e245f4bc))
* separate build from deploy ([e2847ca](https://github.com/discindo/r2lambda/commit/e2847cacf48b0a43652ae55cb7cb3ddd6aa7bc47))
* start, test, and stop a lambda container locally ([d252680](https://github.com/discindo/r2lambda/commit/97862a6718e409b985f6e1e01efb9124ffe4986b))
* start, test, and stop a lambda container locally ([dc213d2](https://github.com/discindo/r2lambda/commit/a499635b9a698009d4b54bbd3dfe22b7ed6f5333))
* try to catch failed invocation, including due to Inactive state ([881f329](https://github.com/discindo/r2lambda/commit/881f329d98b96ffb06a0304b98386554dfa99948))
* update lambda to run on schedule ([7f4472c](https://github.com/discindo/r2lambda/commit/456a6ab7ba173b179b01bfad0fcc8abffeb8b351))
* use magrittr pipe ([6fa8575](https://github.com/discindo/r2lambda/commit/6fa85759d5d60ee9f1d192e497eb8b1c75d81282))
* use stevedore for push to ecr ([3e6641a](https://github.com/discindo/r2lambda/commit/3e6641a1cdf5db235a1d35969833e77582d2eab3))
* use stevedore to create the docker image ([a12a491](https://github.com/discindo/r2lambda/commit/a12a491e987cb256e5e257f704f8dc096af9e83c))


### Bug fixes

* build Lambda images on Amazon Linux 2023 with R 4.6.1 ([d573317](https://github.com/discindo/r2lambda/commit/161a5f9fc76a09462490d1461b4f5ef57faf261e))
* call the docker CLI instead of stevedore ([c711055](https://github.com/discindo/r2lambda/commit/35b96e65825f84c768eae040a89a90500e52f63f)), closes [#11](https://github.com/discindo/r2lambda/issues/11)
* check the image architecture before pushing it ([d74aaf3](https://github.com/discindo/r2lambda/commit/03f7703647fc60238eeead7c6ea8bfbc80b89e7b))
* correct log msg ([79a3df0](https://github.com/discindo/r2lambda/commit/79a3df0cce63ea9f535102c8d701eceb7a6eac49))
* don't create an ECR repository when deploying ([a669cef](https://github.com/discindo/r2lambda/commit/6d20439192488c1fb28f3232f1ddc5647e630069))
* don't need to check for aws cli anymore ([673b69c](https://github.com/discindo/r2lambda/commit/673b69c7720182b3c6bf87d16a5c963d2fd2f741))
* inspect the same image that deploy_lambda pushes ([ec91125](https://github.com/discindo/r2lambda/commit/26b635b9afd6c9900054b35a24a3a25d402c4e73))
* install from github in readme ([61e6435](https://github.com/discindo/r2lambda/commit/61e643567f470e9d556cc476c00ddddcfe900565))
* look up AWS resources by name instead of listing them ([a99bfc0](https://github.com/discindo/r2lambda/commit/5b7f38d4480e9ce7397365c59862cf655821228d))
* name support files in the Dockerfile as they are copied ([bbf5296](https://github.com/discindo/r2lambda/commit/0766ac1f3d1f63edf80c1edc7e787d935c6fe8dc))
* report AWS errors from invoke_lambda and deploy_lambda ([1ceef40](https://github.com/discindo/r2lambda/commit/28912ffe69b2da60d51723ede8e989b8c23883f7))
* tempdir issue in tests ([28032d9](https://github.com/discindo/r2lambda/commit/ac5a18d58c113fcf01feee7469d92251a8b17a00))


### Documentation

* add Novica Nakov as maintainer and ORCIDs for both authors ([9c20ff7](https://github.com/discindo/r2lambda/commit/6b9b8a3c25ef238b5ff089e7b97d0ad707574aed))
* build ignore docs folder ([8a5af02](https://github.com/discindo/r2lambda/commit/8a5af02af3cd512f79815d8c15e1a64d8992bd7e))
* code of conduct. ([9608875](https://github.com/discindo/r2lambda/commit/96088754bc19dd9a7df75494b2cd4b730d0f4ed8))
* correction in readme ([55b077a](https://github.com/discindo/r2lambda/commit/55b077a32360e4fd2bc57df16edf61619492f80b))
* document role, policy, support folder, system package and architecture options ([e2a5d07](https://github.com/discindo/r2lambda/commit/058096088f9ee4282883852c08c70962ef901490))
* fix moved blog link in lambda-s3 vignette ([4471463](https://github.com/discindo/r2lambda/commit/8c06a4005b8c891b85ae044127470eea81686437))
* manual files ([16ba1e2](https://github.com/discindo/r2lambda/commit/16ba1e261a38186613853031b5096d90864be44d))
* manual files ([4c16601](https://github.com/discindo/r2lambda/commit/4c16601c2b3a4c23447efa52d148401f577d682b))
* manual files and vignette ([f470ef7](https://github.com/discindo/r2lambda/commit/815449e7a6e18a47b8d928e63675af99d26fab4b))
* pkg description ([9b778d1](https://github.com/discindo/r2lambda/commit/9b778d1f880d4ccf29ac6823b63de0c3ebdb6625))
* typo ([41386a6](https://github.com/discindo/r2lambda/commit/41386a64fb6c76979dff7df16fdde333fd420882))
* update readme ([a002958](https://github.com/discindo/r2lambda/commit/71927624c861f1f145aa3445a0b95055b26d4d9d))
* update readme ([f89c595](https://github.com/discindo/r2lambda/commit/f89c5957604c8a2ceec9334d959ca691d150add1))
