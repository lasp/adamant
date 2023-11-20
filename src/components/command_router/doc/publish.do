component=`find ../. -maxdepth 1 -mindepth 1 -name "*.component.yaml" | head -n 1 | xargs basename`
component=`basename $component ".component.yaml"`
redo build/pdf/$component.pdf
cp -f build/pdf/$component.pdf .
