
#!/bin/bash
# Let's first look at the current state
cd /home/bpsa/app/dbdesigner-fork

# Check if the backup exists
if [ -f "src/UITestRunner.pas.bak" ]; then
    echo "Backup exists"
fi

echo "Current UITestRunner.pas line count: $(wc -l < src/UITestRunner.pas)"
