run:
    cargo run --profile mid
    
web:
    trunk serve

web_build:
    trunk build --release
    rm -rf ./docs
    mkdir docs
    cp -r ./dist/* ./docs/ 
