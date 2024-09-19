
import m from "mithril";
import { Vnode } from "mithril";
import JSONbig from "json-bigint";

import { Protocol, Node, SceneProp } from "./protocol";

import _ from "lodash";

function PropView(): m.Component<{prop: SceneProp}> {

    let messages = [];
    let unsub = null;
    let value = "";

    return {
        oncreate(vnode) {
            const prop = vnode.attrs.prop;

            if("Value" in prop) {

            } else if("Dispatch" in prop) {
                
                unsub = prop.Dispatch.watch((message) => {
                    messages.push({
                        tx: false,
                        time: new Date(),
                        data: JSON.stringify(message),
                    });
                });

            } else if("Variable" in prop) {
                
            } else {
                throw new Error("uh oh");
            }

        },
        onremove() {
            if(unsub) {
                unsub();
            }
        },
        view(vnode) {
            const prop: SceneProp = vnode.attrs.prop;

            if("Value" in prop) {
                return m("div", JSONbig.stringify(prop.Value));
            } else if("Dispatch" in prop) {
                const d = prop.Dispatch;

                return m(`.d-flex.flex-row.align-items-center`, [
                    d.send && [
                        m(`textarea`, {
                            rows: 4,
                            cols: 20,
                            value,
                            id: `${d.id}-input`,
                            key: `${d.id}-input`,
                            oninput: (evt: any) => {
                                value = evt.target.value;
                            }
                        }),
                        m(`input.ml-3`, {
                            type: "button",
                            value: "Send",
                            key: `${d.id}-send`,
                            onclick: () => {
                                d.send(JSONbig.parse(value));
                                messages.push({
                                    tx: true,
                                    time: new Date(),
                                    data: value
                                });
                                value = "";
                            }
                        })
                    ],
                    m(`.d-flex.flex-column.card.ml-3.align-self-stretch.position-relative.flex-fill`, { style: { flex: "1", overflowY: "scroll"} }, (
                        m(`.position-absolute`, { style: { left: 0 } }, (
                            _(messages)
                                .map(message => m(`.d-flex.flex-row.align-items-center.mb-2`, [
                                    m(`i.mr-2.fa.${message.tx ? "fa-arrow-up" : "fa-arrow-down"}`),
                                    m(`.mr-2`,`${message.time.toLocaleTimeString()}`),
                                    message.data,
                                ]))
                                .reverse()
                                .value()
                        ))
                    ))
                ]);
            } else if("Variable" in prop) {
                const v = prop.Variable;

                return m(`.col`, { style: { width: "10em" } }, [
                    "variable"
                ]);
            } else {
                throw new Error("uh oh");
            }

        }
    };
}

const NodeView = {
    view(vnode: Vnode<{node: Node}>) {
        const node = vnode.attrs.node;

        return m(`div.d-flex.flex-column.ml-3`, [
            m(`span`, [
                "Path:",
                m(`strong.ml-1`, node.path),
            ]),
            m(`span`, [
                "Scene:",
                m(`strong.ml-1`, node.scene),
            ]),

            Object.keys(node.meta).length > 0 && [
                "Meta:",
                m(`.ml-3`, { style: { width: "fit-content", display: "grid", gridTemplateColumns: "auto auto", gridColumnGap: "10px" } }, (
                    _(node.meta)
                        .entries()
                        .sortBy(0)
                        .map(([name, prop]) => [
                            m(`div`, { style: { gridColumn: 1 } }, name),
                            m(`div`, { style: { gridColumn: 2 } }, JSON.stringify(prop)),
                        ])
                        .value()
                ))
            ],

            Object.keys(node.props).length > 0 && [
                "Properties:",
                m(`.ml-3`, { style: { width: "fit-content", display: "grid", gridTemplateColumns: "auto auto", gridColumnGap: "10px", gridRowGap: "10px" } }, (
                    _(node.props)
                        .entries()
                        .sortBy(0)
                        .map(([name, prop]) => [
                            m(`div`, { style: { gridColumn: 1, key: `${name}-name` } }, name),
                            m(`div`, { style: { gridColumn: 2, key: `${name}-prop` } }, [
                                m(PropView, { prop })
                            ]),
                        ])
                        .value()
                ))
            ],

            Object.keys(node.children).length > 0 && [
                "Children:",
                m(`.ml-3`, {
                    style: {
                        display: "grid"
                    }
                }, (
                    _(node.children)
                        .entries()
                        .sortBy(0)
                        .map(([name, node]) => [
                            m(`div`, { style: { col: 1 } }, name),
                            m(`div`, { style: { col: 2 } }, [
                                m(NodeView, { node, key: node.path })
                            ]),
                        ])
                        .value()
                ))
            ],
        ]);
    }
};

function uuidv4() {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

export const NodeViewer = () => {

    let url = `ws://localhost:8080/api/v1/connect/${uuidv4()}`;
    let protocol: Protocol = null;

    return {
        view() {
            const icon = ({
                [-1]: "fa-question",
                [WebSocket.CONNECTING]: "fa-spinner",
                [WebSocket.OPEN]: "fa-check",
                [WebSocket.CLOSING]: "fa-spinner",
                [WebSocket.CLOSED]: "fa-times",
            })[protocol?.websocket ? protocol.websocket.readyState : -1];

            return m(`.col.mx-3.my-3`, [
                m(`.row`, [
                    m("input", {
                        type: "text",
                        style: { width: "40em" },
                        value: url,
                        onchange: (evt: any) => url = evt.target.value,
                    }),
                    m(`button.ml-1`, {
                        onclick: () => {
                            protocol = new Protocol(url);
                        }
                    }, "Connect"),
                    m(`button.ml-1`, {
                        onclick: () => {
                            protocol.disconnect();
                        }
                    }, "Disconnect"),
                    m(`i.ml-1.fa.${icon}`, { style: { display: "flex", alignItems: "center" } })
                ]),

                m(``, (
                    protocol?.tree && m(NodeView, {
                        node: protocol.tree,
                        key: protocol.tree.path,
                    })
                ))
            ])
        }
    }
};
