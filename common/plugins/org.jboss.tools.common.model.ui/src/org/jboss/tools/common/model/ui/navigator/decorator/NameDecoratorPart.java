package org.jboss.tools.common.model.ui.navigator.decorator;

import org.jboss.tools.common.model.XModelObject;

public class NameDecoratorPart extends DecoratorPart {
	
	public NameDecoratorPart() {
		super("name");
	}

	public String getLabelPart(XModelObject object) {
		return "" + object.getPresentationString(); 
	}

}
