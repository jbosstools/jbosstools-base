/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.meta.action.impl;

import org.w3c.dom.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.impl.*;
import org.jboss.tools.common.model.XModelObjectConstants;

public class XEntityDataImpl implements XEntityData, XMetaDataConstants {
    private String entityname;
    private XModelEntity entity = null;
    private XAttributeDataImpl[] attributes;
    private boolean mandatory;
    private boolean references = false;

    public XEntityDataImpl() {}

    public boolean getMandatoryFlag() {
        return mandatory;
    }

    public XAttributeData[] getAttributeData() {
        return attributes;
    }

    public XModelEntity getModelEntity() {
        return (references) ? entity : (entity = loadReference());
    }

    private XModelEntity loadReference() {
        references = true;
        XModelMetaData meta = XModelMetaDataImpl.getInstance();
        return (meta == null) ? null : meta.getEntity(entityname);
    }

    public void setDefaultValues() {
        for (int i = 0; i < attributes.length; i++)
          attributes[i].setDefaultValue();
    }

    public void load(Element el) {
        entityname = el.getAttribute("EntityName"); //$NON-NLS-1$
        mandatory = XModelObjectConstants.YES.equals(el.getAttribute("Mandatory")); //$NON-NLS-1$
        Element[] cs = XMetaDataLoader.getChildrenElements(el, "AttributeData"); //$NON-NLS-1$
        attributes = new XAttributeDataImpl[cs.length];
        for (int i = 0; i < cs.length; i++) {
            attributes[i] = new XAttributeDataImpl();
            attributes[i].setEntityData(this);
            attributes[i].load(cs[i]);
        }
    }
    
    public String getEntityName() {
    	return entityname;
    }

    /**
     * 
     * @param data (non-translatable)
     * @return
     */
    public static XEntityDataImpl create(String[][] data) {
        XEntityDataImpl impl = new XEntityDataImpl();
        impl.entityname = data[0][0];
        impl.mandatory = data[0].length > 1 && XModelObjectConstants.YES.equals(data[0][1]);
        impl.attributes = new XAttributeDataImpl[data.length - 1];
        for (int i = 0; i < impl.attributes.length; i++) {
            impl.attributes[i] = XAttributeDataImpl.create(impl.entityname,
                            data[i + 1][0],
                            (data[i + 1].length > 1 && XModelObjectConstants.YES.equals(data[i + 1][1])));
            impl.attributes[i].setEntityData(impl);
        }
        return impl;
    }
    
    private XAttributeData find(String name) {
    	for (int i = 0; i < attributes.length; i++) 
    		if(attributes[i].getAttribute().getName().equals(name)) return attributes[i];
    	return null;
    }

	public String getValue(String name) {
		XAttributeData d = find(name);
		return (d == null) ? null : d.getValue();
	}
	
	public void setValue(String name, String value) {
		XAttributeData d = find(name);
		if(d != null) d.setValue(value);
	}

}

