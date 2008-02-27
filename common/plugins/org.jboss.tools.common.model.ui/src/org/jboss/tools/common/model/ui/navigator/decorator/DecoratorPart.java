package org.jboss.tools.common.model.ui.navigator.decorator;

import org.jboss.tools.common.model.XModelObject;

public class DecoratorPart {
	String value;
	
	public DecoratorPart(String value) {
		this.value = value;
	}
	
	public String getLabelPart(XModelObject object) {
		return value == null ? "" : value; 
	}

}
