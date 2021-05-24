export function addSmallRoomElement(Element, ElementView, System) {
    class SmallRoomElement extends Element {
        static types() {
            return {
                System: {
                    cls: System,
                    write: (c) => c.definitions,
                    read: (definitions) => {
                        let s = new System();
                        s.addDOM();
                        s.definitions = definitions;
                        s.redefineAll();
                        return s;
                    }
                }
            };
        }

        beRoot() {
            this._system = new System();
            this._system.addDOM();
            this._system.setRoot(this);
        }

        addSTExpander(system, name) {
            if (system) {
                this.setCode("smallroom.Expander");
                this.call("Expander", "addSTExpander", system, name);
            }
        }

        get system() {
            if (this._system) {return this._system;}

            if (!this.parentNode) {return null;}
            return this.parentNode.system;
        }

        static viewClass() {return SmallRoomView;}

        get stClass() {
            return "Element";
        }

        get style() {
            return {stClass: 'Style', self: this._style};
        }
    }

    class SmallRoomView extends ElementView {
    }

    SmallRoomElement.register("SmallRoomElement");
    Element.knownElements["SmallRoomElement"] = SmallRoomElement;
}
