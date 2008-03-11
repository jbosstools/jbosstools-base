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
package org.jboss.tools.common.verification.test;

import junit.framework.TestCase;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VManager;
import org.jboss.tools.common.verification.vrules.VResult;
import org.jboss.tools.common.verification.vrules.VRuleSet;

public class VerificationTest extends TestCase {
	
	public VerificationTest() {}

	protected void setUp() throws Exception {
	}

	public void testVerificationModel() {
		XModel model = PreferenceModelUtilities.getPreferenceModel();
		XModelObject vm = model.getByPath("Verification Manager");
		assertTrue("Verification Manager model object is not found", vm != null);
		VManager m = VHelper.getManager();
		assertTrue("Verification Manager is not found", m != null);

		XModelObject[] os = vm.getChildren();
		assertTrue("No rule sets loaded.", os.length > 0);

		VRuleSet[] s = m.getRuleSets();

		assertTrue("Inconsistency found:\n" 
				+ "   Verification Manager has " 
				+ s.length + " has rule sets;\n"
				+ "   Verification Manager model object has " + os.length + " children.\n"
				+ "These numbers must be equal.", s.length == os.length);
		
		String entity = "Test_1";
		XModelObject o = model.createModelObject(entity, null);
		assertTrue("Cannot create test object " + entity, o != null);
		
		o.setAttributeValue("name", "test");
		XModelObject parent = model.getRoot();
		boolean b = parent.addChild(o);
		assertTrue("Cannot add test object to model " + entity, b);
		
		VResult[] result = VerificationUtil.doTestVerification(o);
		
		assertTrue("Rule failed at correct value " + entity, result == null || result.length == 0);
		
		// Now let us set value that must be reported as a problem.
		o.setAttributeValue("testable", "false");
		result = VerificationUtil.doTestVerification(o);

		assertTrue("Rule passed at incorrect value " + entity, result == null || result.length > 0);
		parent.removeChild(o);
	}
	
}
