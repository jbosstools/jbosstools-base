package org.jboss.tools.common.model.ui.navigator.decorator;

import org.jboss.tools.common.model.XModelObject;

public class AttributeDecoratorPart extends DecoratorPart {
	
	public AttributeDecoratorPart(String value) {
		super(value);
	}

	public String getLabelPart(XModelObject object) {
		String v = object.getAttributeValue(value);
		return v == null ? "{" + value + "}" : v; 
	}

}
