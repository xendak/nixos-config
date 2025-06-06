#!/usr/bin/env bash
# This script initializes a new Zig project by replacing {{projectName}} placeholders

# Ask the user for the project name
echo "Enter your project name:"
read -r project_name

if [ -z "$project_name" ]; then
  echo "No project name provided, using 'my-zig-project'"
  project_name="my-zig-project"
fi

echo "Initializing project as: $project_name"

# Replace {{projectName}} in all files
find . -type f -not -path "*/\.*" -not -path "*/zig-cache/*" -not -path "*/zig-out/*" -exec sed -i "s/{{projectName}}/$project_name/g" {} \;

# Also rename instances of 'template' in default.nix
sed -i "s/pname = \"template\"/pname = \"$project_name\"/g" default.nix
sed -i "s/description = \"A Zig application template\"/description = \"$project_name - A Zig application\"/g" default.nix

# Update build.zig if it exists
if [ -f build.zig ]; then
  sed -i "s/name = \"template\"/name = \"$project_name\"/g" build.zig
else
  cat > build.zig << EOF
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "$project_name",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    // Add raylib as a dependency
    exe.linkSystemLibrary("raylib");
    exe.linkLibC();

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(exe_tests).step);
}
EOF
fi
