
import m from "mithril";
import _ from "lodash";
import JSONBig from "json-bigint"; 

export type PermissionSet =
    { WhiteList: Uuid[] } |
    { BlackList: Uuid[] };

export type Unsubscribe = (() => void);

type DispatchWatcher = ((message: any) => void);
export type Dispatch = {
    id: Uuid;

    send?(message: any): void;

    watch(x: DispatchWatcher): Unsubscribe;
};

type VariableWatcher = ((v: Variable, val: VariableValue) => void);
export type Variable = {
    id: Uuid;

    value: VariableValue;
    set?(value: VariableValue): void;

    watch(x: ((v: Variable, val: VariableValue) => void)): Unsubscribe;
};

export type VariableData = {
    id: Uuid;
    read: PermissionSet | null;
    write: PermissionSet | null;
};

export type SceneProp =
    { Value: any } |
    { Dispatch: Dispatch } |
    { Variable: Variable };

export type Node = {
    path: string,
    scene: string,
    props: Record<string, SceneProp>,
    meta: Record<string, any>,
    children: Record<string, Node>,
    visibility: PermissionSet | null,
};

type ScenePropData =
    { Value: any } |
    { Dispatch: { id: Uuid, rx: boolean, tx: boolean } } |
    { Variable: { id: Uuid, rx: boolean, tx: boolean } };

type NodeData = {
    path: string,
    scene: string,
    props: Record<string, ScenePropData>,
    meta: Record<string, any>,
};

type NodeDataPatch = {
    path: string,
    scene: string,
    props: Record<string, PatchOperation<ScenePropData>>,
    meta: Record<string, PatchOperation<any>>,
};

type PatchOperation<T> = 
    "Delete" |
    { Insert: T };

type NodeDiff =
    "Same" |
    "Delete" |
    { Merge: { data: Partial<NodeDataPatch>, children: Record<string, NodeDiff> } } |
    { Replace: { data: NodeData, children: Record<string, NodeDiff> } };

type Uuid = string;

type FromServerMessage =
    { NodeDelta: { diff: NodeDiff, default_values: Record<Uuid, VariableValue> } } |
    { Dispatch: [Uuid, any] } |
    { Variable: [Uuid, VariableValue] } |
    { Ping: { last_ping?: bigint, data: number[], tag: number[] } };

type VariableValue =
    { Int: bigint[] } |
    { Float: number[] };

type FromClientMessage =
    { Dispatch: [Uuid, any] } |
    { Variable: [Uuid, VariableValue] } |
    { Pong: { client_time: bigint, data: number[], tag: number[] } };

type NodeState = {
    data: NodeData,
    props: Record<string, ScenePropData>,
    children: Record<string, NodeState>,
};

export class Protocol {

    websocket: WebSocket;

    public tree: Node | null;

    dispatches: Record<Uuid, [WeakRef<Dispatch>, DispatchWatcher[]]>;
    variables: Record<Uuid, [WeakRef<Variable>, VariableWatcher[]]>;
    ping: bigint | null;

    constructor(url: string) {
        this.websocket = new WebSocket(url);

        this.websocket.addEventListener('message', (data) => {
            this.handle(JSONBig.parse(data.data));
            m.redraw();
        });
        this.websocket.addEventListener('open', m.redraw);
        this.websocket.addEventListener('error', m.redraw);
        this.websocket.addEventListener('close', m.redraw);

        this.tree = null;
        this.dispatches = {};
        this.variables = {};
        this.ping = null;
    }

    async disconnect() {
        this.websocket.close();
        this.tree = null;
    }

    handle(message: FromServerMessage) {
        console.log("recv:", message);

        if ("NodeDelta" in message) {
            const old = this.tree;

            this.tree = this.patch(this.tree, message.NodeDelta.diff, message.NodeDelta.default_values);

            delta_node_tree(old, this.tree);
        } else if ("Dispatch" in message) {
            const [uuid, value] = message.Dispatch;

            const [d2, w] = this.dispatches[uuid];

            const d = d2.deref();

            if (d) {
                w.forEach(f => f(value));
            } else {
                delete this.dispatches[uuid];
            }
        } else if ("Variable" in message) {
            const [uuid, value] = message.Variable;

            const [v2, w] = this.variables[uuid];

            const v = v2.deref();

            if (v) {
                v.value = value;

                w.forEach(f => f(v, value));
            } else {
                delete this.variables[uuid];
            }
        } else if ("Ping" in message) {
            const ping = message.Ping;

            this.ping = ping.last_ping || null;
            console.log("ping (ms): ", ping.last_ping ? (Number(this.ping) / 1_000_000).toString() : null);

            this.send({
                Pong: {
                    client_time: BigInt((new Date()).getTime()) * BigInt(1_000_000),
                    data: ping.data,
                    tag: ping.tag
                }
            });
        } else {
            throw new Error("uh oh");
        }
    }

    public send(message: FromClientMessage) {
        console.log("send:", message);
        this.websocket.send(new TextEncoder().encode(JSONBig.stringify(message)));
    }

    patch(node: Node | null, diff: NodeDiff, default_values: Record<Uuid, VariableValue>): Node | null {
        if (diff === "Same") {
            return node;
        } else if (diff === "Delete") {
            return null;
        } else if ("Replace" in diff) {

            const data = diff.Replace.data;

            node = {
                path: data.path,
                scene: data.scene,
                props: Object.fromEntries(_(data.props).mapValues(x => this.get_prop(x, default_values)).toPairs().value()),
                meta: data.meta,
                children: {},
                visibility: {
                    BlackList: []
                }
            };

            if(diff.Replace.children) {
                for (const [key, child_diff] of Object.entries(diff.Replace.children)) {
                    if(child_diff === "Delete") {
                        delete node.children[key];
                    } else {
                        let child = node.children[key];
    
                        child = this.patch(child, child_diff, default_values);

                        if (child) {
                            node.children[key] = child;
                        }
                    }
                }
            }

            return node;
        } else if ("Merge" in diff) {
            console.assert(node != null);

            if (diff.Merge.data) {
                const data = diff.Merge.data;

                if (data.scene) {
                    node.scene = data.scene;
                }
    
                if (data.props) {
                    for(const [key, op] of Object.entries(data.props)) {
                        if(op === "Delete") {
                            delete node.props[key];
                        } else if ("Insert" in op) {
                            node.props[key] = this.get_prop(op.Insert, default_values);
                        } else {
                            throw new Error("uh oh");
                        }
                    }
                }
    
                if (data.meta) {
                    for(const [key, op] of Object.entries(data.meta)) {
                        if(op === "Delete") {
                            delete node.meta[key];
                        } else if ("Insert" in op) {
                            node.meta[key] = op.Insert;
                        } else {
                            throw new Error("uh oh");
                        }
                    }
                }
            }

            if(diff.Merge.children) {
                for (const [key, child_diff] of Object.entries(diff.Merge.children)) {
                    if(child_diff === "Delete") {
                        delete node.children[key];
                    } else {
                        let child = node.children[key];
    
                        child = this.patch(child, child_diff, default_values);

                        if (child) {
                            node.children[key] = child;
                        }
                    }
                }
            }

            return node;
        } else {
            throw new Error("uh oh");
        }
    }

    get_prop(data: ScenePropData, defaults: Record<Uuid, VariableValue>): SceneProp {
        if ("Value" in data) {
            return { Value: data.Value };
        } else if ("Dispatch" in data) {
            return { Dispatch: this.get_dispatch(data.Dispatch.id, data.Dispatch.tx) };
        } else if ("Variable" in data) {
            return { Variable: this.get_variable(data.Variable.id, data.Variable.tx, defaults) };
        } else {
            throw new Error("uh oh");
        }
    }

    get_dispatch(id: Uuid, tx: boolean): Dispatch {

        let disp = this.dispatches[id]?.[0]?.deref();

        if (disp) {
            return disp;
        }

        const watchers = [];

        const proto = this;

        disp = {
            id,

            watch(x) {
                watchers.push(x);
                return () => _.remove(watchers, w => w === x);
            },
        };

        if (tx) {
            disp.send = function (message) {
                proto.send({
                    Dispatch: [id, message]
                });
            }
        }

        this.dispatches[id] = [new WeakRef(disp), watchers];

        return disp;
    }

    get_variable(id: Uuid, tx: boolean, defaults: Record<Uuid, VariableValue>): Variable {

        let variable = this.variables[id]?.[0]?.deref();

        if (variable) {
            return variable;
        }

        const watchers = [];

        const proto = this;

        variable = {
            id,

            value: defaults[id],
            watch(x) {
                watchers.push(x);
                return () => _.remove(watchers, w => w === x);
            },
        };

        if (tx) {
            variable.set = function (value) {
                variable.value = value;
                proto.send({
                    Variable: [id, value]
                });
            }
        }

        this.variables[id] = [new WeakRef(variable), watchers];

        return variable;
    }

}

type DiffDelta = {
    a: any;
    b: any;
    key: string;
};

const KEY_REGEX = /^[a-zA-Z_]+[a-zA-Z_0-9]*$/;

function diff(a: any, b: any, path: string[] = []): DiffDelta[] {
    if(a === b) {
        return [];
    }

    if(typeof a !== typeof b || _.isNil(a) !== _.isNil(b) || _.isArray(a) !== _.isArray(b)) {
        return [
            {a, b, key: path.join("")}
        ];
    }

    const diffs = [];

    if(_.isArray(a) && _.isArray(b)) {
        const size = Math.max(a.length, b.length);

        for(var i = 0; i < size; i++) {
            diff(a[i], b[i], [...path, `[${i}]`]).forEach(d => diffs.push(d));
        }
    } else {
        const keys = _.union(_.keys(a), _.keys(b));

        for(const key of keys) {
            let pretty_key: string;

            if(KEY_REGEX.test(key)) {
                pretty_key = `.${key}`;
            } else {
                pretty_key = `["${key.replaceAll('"', "\\\"")}"]`;
            }

            diff(a[i], b[i], [...path, pretty_key]).forEach(d => diffs.push(d));
        }
    }

    return diffs;
}

function delta_node_tree(old: Node | null, next: Node | null) {

    const diffs = diff(old, next);

    if(diffs.length > 0) {
        console.group(new Date(), "Node Update");

        for(const diff of diffs) {
            console.log(diff.key, diff.a, "->", diff.b);
        }
    
        console.groupEnd();
    } else {
        console.log(new Date(), "No-op Node Update");
    }

}
