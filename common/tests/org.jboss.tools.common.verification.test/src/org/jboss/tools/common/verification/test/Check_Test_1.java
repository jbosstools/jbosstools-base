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

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.verification.vrules.VAction;
import org.jboss.tools.common.verification.vrules.VObject;
import org.jboss.tools.common.verification.vrules.VResult;
import org.jboss.tools.common.verification.vrules.VRule;

public class Check_Test_1 implements VAction {
	protected VRule rule;
	protected XModel model;

	public VResult[] check(VObject object) {
		String attributeName = rule.getProperty("attribute");
		String value = (String)object.getAttribute(attributeName);
		if("false".equals(value)) {
			fire(object, "problem1", attributeName, null);
		}
		System.out.println(object);
		return null;
	}

	public VRule getRule() {
		return rule;
	}

	public void setRule(VRule rule) {
		this.rule = rule;
	}
	
    protected VResult[] fire(VObject object, String id, String attr, String info) {
        Object[] os = (info == null) ? new Object[] {object, object}
                      : new Object[] {object, object, info};
        VResult result = rule.getResultFactory().getResult(id, object, attr, object, attr, os);
        return new VResult[] {result};
    }

}
