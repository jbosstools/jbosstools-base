package org.jboss.tools.common.model.options.impl;

import org.jboss.tools.common.meta.action.impl.handlers.DefaultRemoveHandler;
import org.jboss.tools.common.model.XModelObject;

public class DeleteGroupHandler extends DefaultRemoveHandler {
	
	public DeleteGroupHandler() {}

	public boolean isEnabled(XModelObject object) {
    	if(!super.isEnabled(object)) return false;
    	if(!(object instanceof SharableElementImpl)) return false;
    	SharableElementImpl e = (SharableElementImpl)object;
    	String name = e.getAttributeValue("name");
    	if(!(e.getParent() instanceof SharableElementImpl)) return false;
    	SharableElementImpl p = (SharableElementImpl)e.getParent();
    	return p.canRemoveChild(name);
	}

}
