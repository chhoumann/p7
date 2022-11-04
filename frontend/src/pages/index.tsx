import Link from "next/link";
import React from "react";
import Layout from "../components/layout";
import SolveProblem from "../pages/problem/[problemId]/index";

export default function IndexPage() {
    return (
        <Layout title={"Home"}>
            <div className="background-content">
                <h3 className="landing-title-first max-md">Welcome to</h3>
                <h3 className="landing-title-second max-sm">The AAU Programming</h3>
                <h3 className="landing-title-third max-sm">Paradigms Course</h3>
            </div>
        </Layout>
    );
}

