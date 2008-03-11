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
package org.jboss.tools.common.model.ui.templates.model;

import org.jboss.tools.common.model.ui.templates.configuration.MetaConfiguration;

public class MetaElementFactory {
	public static MetaElementFactory instance = new MetaElementFactory();
	
	public MetaConfiguration createConfiguraton(MetaConfiguration parent) {
		MetaConfiguration c = new MetaConfiguration();
		c.setParent(parent);
		return c;
	}

	public MetaGroup createGroup(MetaGroup parent) {
		MetaGroup c = new MetaGroup();
		c.setParent(parent);
		return c;
	}

	public MetaClassTemplate createClassTemplate(MetaClassTemplate parent) {
		MetaClassTemplate c = new MetaClassTemplate();
		c.setParent(parent);
		return c;
	}
	
	public MetaValue createValue(MetaValue parent) {
		MetaValue v = new MetaValue();
		v.setParent(parent);
		return v;
	}

	public MetaValueList createValueList(MetaValueList parent) {
		MetaValueList v = new MetaValueList();
		v.setParent(parent);
		return v;
	}

}
