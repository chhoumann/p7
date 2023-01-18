import React from "react";
import CodeMirror, {
    ReactCodeMirrorProps,
    ReactCodeMirrorRef,
} from "@uiw/react-codemirror";
import { githubLight } from "@uiw/codemirror-theme-github";
import { StreamLanguage } from "@codemirror/language";
import { haskell } from "@codemirror/legacy-modes/mode/haskell";
import { EditorView } from "@codemirror/view";

function CodeEditor({
    setCode,
    ...props
}: ReactCodeMirrorProps &
    React.RefAttributes<ReactCodeMirrorRef> & {
        setCode: (str: string) => void;
    }) {
    return (
        <CodeMirror
            theme={githubLight}
            extensions={[
                StreamLanguage.define(haskell),
                EditorView.updateListener.of((e) => {
                    setCode(e.state.doc.toString());
                }),
            ]}
            autoFocus={true}
            style={{ fontSize: "1.2rem" }}
            {...props}
        />
    );
}

export default CodeEditor;
