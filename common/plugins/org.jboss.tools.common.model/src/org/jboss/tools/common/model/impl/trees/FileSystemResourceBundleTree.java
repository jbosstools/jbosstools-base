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
package org.jboss.tools.common.model.impl.trees;

import java.util.*;
import org.jboss.tools.common.model.XModelObject;

public class FileSystemResourceBundleTree extends FileSystemResourceTree {
	
	public void setConstraint(Object object) {
		extensions = new HashSet<String>();
		extensions.add("properties");
	}

	public String getValue(XModelObject object) {
		String p = getPath(object);
		if(p != null && p.endsWith(".properties")) {
			p = p.substring(1, p.length() - 11).replace('/', '.'); 
		}
		return p;
	}

	public XModelObject find(String value) {
		return (value == null || value.length() == 0) ? null : 
		       model.getByPath("/" + value.replace('.', '/') + ".properties");
	}

}
