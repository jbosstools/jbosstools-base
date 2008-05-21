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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.ui.vrules.wizard.VRuleTipFactory;

public class RuntimeRuleSetWrapper extends RuntimeItemWrapper {
	protected VRuleSet set;
	RuntimeItemWrapper[] children = new RuntimeItemWrapper[0];

	public RuntimeRuleSetWrapper(VRuleSet set) {
		this.set = set;
	}

	public String getPresentation() {
		return set.getName();
	}

	public Object getObject() {
		return set;
	}

	public void updateStatus() {
		if(children.length == 0) return;
		int min = 2, max = 0;
		for (int i = 0; i < children.length; i++) {
			int s = children[i].getStatus();
			if(s < min) min = s;
			if(s > max) max = s;
		}
		if(max == 2) setStatus(2);
		else if(min == 1) setStatus(1);
	}

	public VRule[] getRules() {
		if(children == null) return new VRule[0];
		VRule[] rs = new VRule[children.length];
		for (int i = 0; i < rs.length; i++) {
			VRule[] x = children[i].getRules();
			if(x.length > 0) rs[i] = x[0];
		}
		return rs;
	}

	public String getTip() {
		return VRuleTipFactory.getRuleTip(set);
	}

}
