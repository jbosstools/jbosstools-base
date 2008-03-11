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
package org.jboss.tools.common.meta.ui.editor;

import java.util.TreeSet;

import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.model.XModel;

public class MappingHelper {
	
	public static MappingHelper getMappingHelper(XModel model) {
		MappingHelper h = (MappingHelper)model.getManager("MappingHelper");
		if(h == null) {
			h = new MappingHelper();
			h.setModel(model);
		}		
		return h;
	}
	
	XModel model;
	
	void setModel(XModel model) {
		this.model = model;
	}
	
	public String[] getList(String mappingName) {
		XMapping mapping = model.getMetaData().getMapping(mappingName);
		if(mapping == null) return new String[0];
		String[] ss = mapping.getKeys();
		TreeSet set = new TreeSet();
		for (int i = 0; i < ss.length; i++) set.add(ss[i]);
		
		return (String[])set.toArray(new String[0]);
	}

}
