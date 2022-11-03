# ghapps

GitHub *apps* provide a powerful way to manage fine grained programmatic access to specific git repositories, without having to create dummy users, and which are safer than PATs for automated tasks. This package extends gh to let you authenticate and interact with the GitHub API in R on behalf of an app.


## Details

Instead of authenticating as a user, you can also authenticate with the GitHub API as a "GitHub app". An app is a first class actor within GitHub. This means it has its own permissions to specific repositories, without being tied to any particular user account. In fact, you could think of an app as a special type of dummy user with a specific purpose. Don't be too intimidated by the word 'app', it is mainly an authentication concept. Any program that authenticates with GitHub, such as an R script, can be considered an app.

### Creating a GitHub app

To register a new app go to: https://github.com/settings/apps/new. You may register as many apps as you like, with different permissions, for different purposes.

You can choose if the the app is public (to allow others to give the app access to their repo by "installing" the app) or if the app is only for your own account. The latter is basically a safe way to provide limited access to a single repository, without exposing your user account in any way.

### Acting on behalf of an app in R

We can interact with [most of the GitHub API](https://docs.github.com/en/rest/overview/endpoints-available-for-github-apps) on behalf of an app in exactly the same way as a regular user. The only difference is that instead of a personal access token (PAT) we generate a temporary app token, which looks very similar. From here we use gh() in exactly the same way as usual.

Authentication on behalf of an app is a two step process. First you need to generate a so-called JWT with gh_app_jwt() using the App-ID and a RSA key file that you can retrieve on GitHub in the app settings. This JWT is only valid for 5 minutes, and is used to generate app tokens for specific target repositories with gh_app_token(). This app token works the same as a PAT, but it is valid for 1 hour and has permission only to the repository that you specified as the installations parameter in gh_app_token(). From here you can use gh() to perform all the operations that the git repository has given your app permission for.

### Using this in CI

A common use case is to authenticate as a github-app inside a github action script, in order to run tasks that require authentication, without the need to use someones personal credentials. To make this easy, you can specify the app-id and private key via environment variables GH_APP_ID and GH_APP_KEY which you can expose as a 'secret' in the CI. The GH_APP_KEY can either be a file path, or simply the verbatim content of the private key (pem) file.
