
import m from "mithril";

export class Layout implements m.ClassComponent {
    view(vnode: m.Vnode<{}, this>): m.Children {
        return m(".container-fluid", [
            m("nav.row.navbar.navbar-expand-md.navbar-dark.bg-dark", [
                m("a.navbar-brand", {
                    href: "#"
                }, [
                    "Logo"
                ]),

                m("ul.navbar-nav", [
                    m("li.nav-item", (
                        m("a.nav-link", {
                            href: "#"
                        }, (
                            "Home"
                        ))
                    ))
                ])
            ]),

            vnode.children,
        ]);
    }
}

/*
<nav class="navbar navbar-expand-md">
	<a class="navbar-brand" href="#">Logo</a>
	<button class="navbar-toggler navbar-dark" type="button" data-toggle="collapse" data-target="#main-navigation">
		<span class="navbar-toggler-icon"></span>
	</button>
	<div class="collapse navbar-collapse" id="main-navigation">
		<ul class="navbar-nav">
			<li class="nav-item">
				<a class="nav-link" href="#">Home</a>
			</li>
			<li class="nav-item">
				<a class="nav-link" href="#">About</a>
			</li>
			<li class="nav-item">
				<a class="nav-link" href="#">Contact</a>
			</li>
		</ul>
	</div>
</nav>
*/
