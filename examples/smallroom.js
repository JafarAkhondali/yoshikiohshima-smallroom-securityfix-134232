class SmallRoomModel {
    init() {
    }

    setEditor(editor) {
        this._set("editor", editor.asElementRef());
        this.subscribe(editor.id, "accept", "compile");
    }

    setConsole(console) {
        this._set("console", console.asElementRef());
        this.subscribe(console.id, "accept", "evaluate");
    }

    compile() {
        let editorRef = this._get("editor");
        if (!editorRef) {return;}
        let editor = this.getElement(editorRef);
        let text = editor.value;
        if (this.system) {
            this.system.compile(text);
        }
    }

    evaluate() {
        let consoleRef = this._get("console");
        if (!consoleRef) {return;}
        let c = this.getElement(consoleRef);
        let text = c.value;
        if (this.system) {
            this.system.evaluate(text);
        }
    }
}

class Expander {
    init() {
        this._set("handlers", {});
        this._style.setProperty("-cards-direct-manipulation", true);
    }

    addSTExpander(system, name) {
        if (system[name] && system[name]['initialize']) {
            system.stInitCall('initialize', {stClass: name, self: this});
        }
    }

    addSTEventListener(event, handler) {
        // let us say that it is always a Symbol object;
        let e = handler.expander;
        let m = handler.string;
        this.addSTListener(event, e, m);
    }

    addSTListener(event, e, m) {
        let key = `${e}.${m}`;
        let handlers = this._get("handlers");

        let array = handlers[event];
        if (!array) {
            handlers[event] = [];
            array = handlers[event];
        }

        array.push(key);
        this.addEventListener(event, "Expander.handleEvent");
    }

    handleEvent(evt) {
        let handlers = this._get("handlers");
        let array = handlers[evt.type];
        let system = this.system;
        array.forEach((key) => {
            let split = key.split('.');
            system.stInitCall(split[1], {stClass: split[0], self: this}, evt);
        });
    }
}

function beSmallRoom(parent, _json, _persistent) {
    parent.setStyleClasses(`

.parent {
    height: 100%;
    width: 100%;
    display: flex;
    flex-direction: column;
}

.playground {
    height: 90%;
    display: flex;
}

#room {
    width: 60%;
    background-color: #F8F8F8;
    height: 100%;
}

#editor {
    height: 100%;
    width: 40%;
}

#console {
    height: 10%;
    border: 1px solid gray;
}

.text-div {
   font-family: sans-serif;
   font-size: 16px;
}

`);

    parent.classList.add("parent");

    let playground = parent.createElement();
    playground.classList.add("playground");
    let room = parent.createElement("SmallRoomElement");
    room.beRoot();
    room.domId = "room";
    room.setCode("smallroom.SmallRoomModel");

    let text = parent.createElement("TextElement");
    text.domId = "editor";
    text.setWidth(1000);// should be adaptable when the parent width changed.
    text.setDefault("sans-serif", 16);
    room.call("SmallRoomModel", "setEditor", text);

    playground.appendChild(room);
    playground.appendChild(text);
    parent.appendChild(playground);

    let console = parent.createElement("TextElement");
    console.domId = "console";
    console.setDefault("sans-serif", 16);

    console.value = `Morph openIn: (document querySelector: #room)`;
    room.call("SmallRoomModel", "setConsole", console);

    text.value = `
Morph instVars: 'angle'.

Morph initialize [
   self addEventListenerFor: 'click' with: #onClick:.
   self style width: 50.
   self style height: 40.
   self style backgroundColor: 'blue'.
   angle := 0.
].

Morph onClick: evt [
   | w |
   w := self style width.
   self style width: w + 10.
   angle := angle + 0.1.
   self rotateTo: angle.
].

Morph class openIn: parent [
   | elem |
   elem := Element new.
   elem addExpander: Morph.
   parent appendChild: elem.
]
`;

    parent.appendChild(console);
}

export const smallroom = {
    classes: [SmallRoomModel, Expander],
    functions: [beSmallRoom]
};
