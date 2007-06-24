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
package org.jboss.tools.common.verification.ui.vrules.preferences;

import java.util.*;
import org.jboss.tools.common.model.ui.preferences.IPreferencePageExt;
import org.jboss.tools.common.model.ui.wizards.query.list.TreeItemSelectionManager;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.jboss.tools.common.verification.ui.vrules.wizard.*;
import org.jboss.tools.common.verification.ui.vrules.wizard.config.*;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.verification.vrules.core.resources.MarkerClearer;
import org.jboss.tools.common.verification.vrules.*;

public class RulesConfigurationPage extends PreferencePage implements IPreferencePageExt, IWorkbenchPreferencePage {
	protected ConfigSignificanceView significance = new ConfigSignificanceView();
	protected TreeViewer treeViewer;
	protected ConfigRulesProvider provider = new ConfigRulesProvider();
	protected DescriptionManager tip = new DescriptionManager();
	VManager manager;

	protected Control createContents(Composite parent) {
		_init();
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.horizontalSpacing = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 10;
		layout.marginWidth = 10;
		composite.setLayout(layout);
		//composite.setBackground(new Color(null,255,0,0));
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);
		
		Control sc = significance.createControl(composite);
		sc.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		treeViewer = new TreeViewer(composite, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		treeViewer.setContentProvider(provider);
		treeViewer.setLabelProvider(provider);
		treeViewer.setInput(this);
		Control tc = treeViewer.getControl();
		tc.setLayoutData(new GridData(GridData.FILL_BOTH));
		new TreeItemSelectionManager(treeViewer, new Flipper());
		significance.update();
		tip.install(treeViewer.getTree());
		treeViewer.expandToLevel(2);
		return composite;
	}

	void _init() {
//		Object model = PreferenceModelUtilities.getPreferenceModel();
		manager = VHelper.getManager(/*model*/);
		if(manager == null) return;
		significance.setManager(manager);
		VRuleSet[] sets = manager.getRuleSets();
		RuleSetWrapper[] ruleSets = new RuleSetWrapper[(sets == null) ? 0 : sets.length];
		for (int i = 0; i < sets.length; i++) {
			ruleSets[i] = new RuleSetWrapper(sets[i]);
			createChildren(ruleSets[i], sets[i]);
		}
		provider.setRuleSets(ruleSets);
	}

	private void createChildren(RuleSetWrapper ruleSetWrapper, VRuleSet set) {
		ArrayList<ConfigItemWrapper> list = new ArrayList<ConfigItemWrapper>();
		VRuleSet[] ss = set.getRuleSets();
		for (int j = 0; j < ss.length; j++) {
			RuleSetWrapper c = new RuleSetWrapper(ss[j]);
			createChildren(c, ss[j]);
			c.setParent(ruleSetWrapper);
			list.add(c);
		} 
		VRule[] rs = set.getRules();
		for (int j = 0; j < rs.length; j++) { 
			list.add(new RuleWrapper(rs[j], significance, ruleSetWrapper));
		}
		ruleSetWrapper.setChildren((ConfigItemWrapper[])list.toArray(new ConfigItemWrapper[0]));
	}

	public void init(IWorkbench workbench) {
	}

	class Flipper implements TreeItemSelectionManager.Listener {
		public void flip(TreeItem item) {
			ConfigItemWrapper w = (ConfigItemWrapper)item.getData();
			if(!w.isEnabled()) return;
			w.flip();
			treeViewer.refresh(w);
		}		
	}
	
	public boolean performCancel() {
		_cancel();
		return super.performCancel();
	}

	private void _cancel() {
		RuleSetWrapper[] ruleSets = provider.getRuleSets();
		for (int i = 0; i <	ruleSets.length; i++) {
			ruleSets[i].cancel();
		}
	}
	
	

	public void performDefaults() {
		RuleSetWrapper[] ruleSets = provider.getRuleSets();
		for (int i = 0; i <	ruleSets.length; i++) {
				ruleSets[i].setDefaults();
		}

		if (treeViewer != null) {
//        	treeViewer.refresh(ruleSets[i], true);
        	treeViewer.refresh();
//        	ruleSets[i].getParent();
        }

		significance.loadDefaults();
		
		super.performDefaults();
	}
	
	public boolean performOk() {
		significance.commit();
		PreferenceModelUtilities.getPreferenceModel().save();
		if(isDisabled()) {
			MarkerClearer.clearAll();
		}
		return true;
	}
	
	private boolean isDisabled() {
		RuleSetWrapper[] ruleSets = provider.getRuleSets();
		if(ruleSets.length == 0) return true;
		for (int i = 0; i < ruleSets.length; i++) 
		  if(ruleSets[i].isSelected()) return false;
		return true;
	}
	
	public String getTitle() {
		return "Rules Configuration";
	}
	
}
