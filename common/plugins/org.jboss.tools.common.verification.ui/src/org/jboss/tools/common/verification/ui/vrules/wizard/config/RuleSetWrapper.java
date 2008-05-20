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
package org.jboss.tools.common.verification.ui.vrules.wizard.config;

import org.jboss.tools.common.verification.vrules.VRuleSet;
import org.jboss.tools.common.verification.ui.vrules.wizard.VRuleTipFactory;

public class RuleSetWrapper extends ConfigItemWrapper {
	ConfigItemWrapper[] children = new ConfigItemWrapper[0];
	VRuleSet o;
	public RuleSetWrapper(VRuleSet set) {
		o = set;
		isSelectedInitial = o.isEnabled();
	}
	public String getPresentation() {
		return o.getName();
	}
	public boolean isSelected() {
		return o.isEnabled();
	}
	public void cancel() {
		o.setEnabled(isSelectedInitial);
		for (int j = 0; j < children.length; j++) children[j].cancel();
	}
	public void flip() {
		o.setEnabled(!o.isEnabled());
	}
	public void setDefaults() {
		o.setEnabled(true);
		for (int j = 0; j < children.length; j++) children[j].setDefaults();
	}
	public String getTip() {
		return VRuleTipFactory.getRuleTip(o);
	}
	public ConfigItemWrapper[] getChildren() {
		return children; 
	}
	public void setChildren(ConfigItemWrapper[] children) {
		this.children = children; 
	}

}
