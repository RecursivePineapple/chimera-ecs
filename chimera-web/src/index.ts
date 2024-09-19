
import m from "mithril";
import { NodeViewer } from "./node_viewer";
import { Layout } from "./layout";

function withLayout(component: m.ComponentTypes): m.Component {
    return {
        view(vnode) {
            return m(Layout, m(component, vnode.attrs, vnode.children));
        }
    }
}

m.route(document.body, "/", {
    "/": withLayout(NodeViewer)
});
