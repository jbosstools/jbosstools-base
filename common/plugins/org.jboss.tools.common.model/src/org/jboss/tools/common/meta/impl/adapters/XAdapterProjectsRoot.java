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
package org.jboss.tools.common.meta.impl.adapters;

import org.jboss.tools.common.meta.constraint.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class XAdapterProjectsRoot extends XAdapterModelElement {
	static String USE_DEFAULT = "Use Default Path";

	public String getProperty(XProperty object) {
		XModelObject o = (XModelObject)object;
		if("yes".equals(o.getAttributeValue(USE_DEFAULT))) {
			return ModelPlugin.getWorkspace().getRoot().getLocation().toOSString();
		}
		return super.getProperty(object);
	}			

}
