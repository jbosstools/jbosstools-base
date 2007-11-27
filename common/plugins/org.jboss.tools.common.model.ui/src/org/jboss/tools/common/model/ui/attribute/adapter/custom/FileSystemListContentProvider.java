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
package org.jboss.tools.common.model.ui.attribute.adapter.custom;

import java.util.*;
import org.jboss.tools.common.meta.constraint.impl.XAttributeConstraintAList;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultXAttributeListContentProvider;

public class FileSystemListContentProvider extends DefaultXAttributeListContentProvider {
	private XModel model;
	private boolean emptyChoice = false;
	private boolean writeOnly = false;
	
	public void setModel(XModel model) {
		this.model = model;
	}

	protected void loadTags() {
		XModelObject fss = model.getByPath("FileSystems");
		if(fss == null) return;
		XModelObject[] os = fss.getChildren();
		XAttributeConstraintAList c = (XAttributeConstraintAList)attribute.getConstraint();
		loadProperties(c.getValues());
		List<String> list = new ArrayList<String>(os.length+1);
		if (emptyChoice) list.add("");
		for (int i = 0; i < os.length; i++) {
			if (!writeOnly || os[i].isObjectEditable()) {
				list.add(os[i].get("NAME"));
			}
		}
		tags = list.toArray(new String[list.size()]);
		if (emptyChoice) list.add("--EmptyChoice--");
		if (writeOnly) list.add("--WriteOnly--");
		c.setValues((String[])list.toArray(new String[list.size()]));
	}

	private void loadProperties(String[] props) {
		for (int i = 0; i < props.length; i++) {
			if ("--EmptyChoice--".equalsIgnoreCase(props[i])) emptyChoice = true;
			else if ("--WriteOnly--".equalsIgnoreCase(props[i])) writeOnly = true;
		}
	}

}
