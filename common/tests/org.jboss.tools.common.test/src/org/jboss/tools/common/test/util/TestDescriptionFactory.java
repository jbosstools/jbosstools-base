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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.jboss.tools.common.xml.XMLEntityResolver;
import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

/**
 * Utility class that helps to do parametrization of 
 * unit tests by data stored in xml.
 * The xml location can be any place known to a specific unit-test,
 * i.e. inside test project, or inside test plugin.
 * 
 * Each named <test> element can declare any set of name-value pairs
 * in <property> elements, and it is up to the implementation of 
 * a unit test method to request the list of test descriptions 
 * by the name and process them interpreting the properties.
 * 
 * <!ELEMENT tests (test)*>
 * <!ELEMENT test (property)*>
 * <!ATTLIST test name CDATA #REQUIRED>
 * <!ELEMENT property EMPTY>
 * <!ATTLIST property name CDATA #REQUIRED>
 * <!ATTLIST property value CDATA #IMPLIED>
 * 
 * @author V.Kabanovich
 *
 */
public class TestDescriptionFactory {
	IFile file = null;
	Set<TestDescription> testDescriptions = null;
	Map<String, ArrayList<TestDescription>> testDescriptionsMap = new HashMap<String, ArrayList<TestDescription>>();
	
	public TestDescriptionFactory(IFile file) {
		this.file = file;
	}

	/**
	 * Returns all test descriptions declared in file.
	 * @return
	 */
	public Set<TestDescription> getTestDescriptions() {
		if(testDescriptions != null) return testDescriptions;
		testDescriptions = new HashSet<TestDescription>();
		if(file == null || !file.exists()) return null;
		Element tests = XMLUtilities.getElement(file.getLocation().toFile(), XMLEntityResolver.getInstance());
		if(tests == null) return null;
		Element[] ts = XMLUtilities.getChildren(tests, "test");
		for (int i = 0; i < ts.length; i++) {
			TestDescription t = new TestDescription(ts[i]);
			testDescriptions.add(t);
		}		
		return testDescriptions;
	}
	
	/**
	 * Return list of test descriptions with the given name.
	 * @param name
	 * @return
	 */	
	public ArrayList<TestDescription> getTestDescriptions(String name) {
		getTestDescriptions();
		if(testDescriptions == null) return null;
		ArrayList<TestDescription> set = testDescriptionsMap.get(name);
		if(set == null) {
			set = new ArrayList<TestDescription>();
			testDescriptionsMap.put(name, set);
			Iterator<TestDescription> it = testDescriptions.iterator();
			while(it.hasNext()) {
				TestDescription t = it.next();
				if(name.equals(t.getName())) set.add(t);
			}
		}
		return set;
	}

}
