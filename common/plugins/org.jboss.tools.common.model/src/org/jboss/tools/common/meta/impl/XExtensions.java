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
package org.jboss.tools.common.meta.impl;

import java.util.*;
import org.w3c.dom.*;

import org.jboss.tools.common.meta.XModelMetaData;

public class XExtensions implements XMetaDataConstants {
    protected HashMap<String,ArrayList<XModelEntityExtensionImpl>> entities = new HashMap<String,ArrayList<XModelEntityExtensionImpl>>();

    public XExtensions() {}

    public void addExtension(Element element) {
        XModelEntityExtensionImpl extension = new XModelEntityExtensionImpl();
        extension.setElement(element);
        extension.validate();
        String n = extension.getName();
        ArrayList<XModelEntityExtensionImpl> l = getExtensions(n);
        if(l == null) {
            l = new ArrayList<XModelEntityExtensionImpl>();
            entities.put(n, l);
        }
        l.add(extension);
    }

    public ArrayList<XModelEntityExtensionImpl> getExtensions(String entity) {
        return entities.get(entity);
    }
    /**
     * Returns set of names of unresolved extensions.
     * @param meta
     * @return
     */
    public Set test(XModelMetaData meta) {
    	Set<String> set = new HashSet<String>();
    	Iterator it = entities.keySet().iterator();
    	while(it.hasNext()) {
    		String n = it.next().toString();
    		if(meta.getEntity(n) == null) {
    			set.add(n);
    		}
    	}
    	return set;
    }

}
