# Github Repository Sync
This will be a tool to sync the configuration between multiple githubs repository, in order to configure once and sync everywhere.

For now this is a project to help me learn PureScript, it's far away for being feature complete.

# To Develop
Clone the repository

    git clone git@github.com:hrajchert/gh-repo-sync.git

and install dependencies

    bower install
    npm install

make sure to have [purescript installed](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md). If you want to create the binary file execute

    npm run build

To run it

    npm run start



You'll need to create a `config.json with the properties as stated by the [Config object](src/Main.purs). You can issue a github token from the [Personal Access Token](https://github.com/settings/tokens/new) page.


