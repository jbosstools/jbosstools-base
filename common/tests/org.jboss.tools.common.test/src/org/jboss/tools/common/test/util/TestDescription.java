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
package org.jboss.tools.common.test.util;

import java.util.Properties;

import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

/**
 * Test descriptions are contained in xml like this
 * 
 * <tests>
 *     <test name="testName">
 *         <property name="property1" value="value1"/>
 *         <property name="property2" value="value2"/>
 *         ...
 *     </test>
 *     ...
 * </tests>
 * 
 * Several tests can have the same name. A unit-test method
 * can request for the list of them by the name from 
 * TestDescriptionFactory and process the list.
 * 
 * @author V.Kabanovich
 *
 */
public class TestDescription extends Properties {
	private static final long serialVersionUID = 1L;

	String name;
	
	public TestDescription() {}
	
	public TestDescription(Element t) {
		name = t.getAttribute("name");
		Element[] ps = XMLUtilities.getChildren(t, "property");
		for (int i = 0; i < ps.length; i++) {
			String n = ps[i].getAttribute("name");
			String v = ps[i].getAttribute("value");
			if(n != null && v != null) setProperty(n, v);
		}
	}
	
	public String getName() {
		return name;
	}
	
}
