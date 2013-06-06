/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.templates.configuration;

import junit.framework.TestCase;

import org.eclipse.core.resources.ResourcesPlugin;
import org.jboss.tools.common.model.ui.templates.model.MetaClassTemplate;

public class MetaClassTemplateHelperTest extends TestCase {

	public void testCRisReplacedWithSpace() {
		MetaClassTemplate template = MetaClassTemplateHelper.instance.getMetaTemplate(
			ResourcesPlugin.getWorkspace().getRoot().getProject("test" + System.currentTimeMillis()),
			"http://java.sun.com/xml/ns/javaee   http://java.sun.com/xml/ns/javaee/web-facesconfig_1_2.xsd",
			"application-factory/text()[1]");
		assertNotNull(template);
	}

}
