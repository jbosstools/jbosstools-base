/*******************************************************************************
 * Copyright (c) 2007-2015 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.preferences;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.util.ModelUtilities;

/**
 * @author eskimo
 *
 */
public class TabbedPreferencesPage extends PreferencePage implements IWorkbenchPreferencePage {

	Map<String,PreferencePage> map = new HashMap<String,PreferencePage>();
	
	public TabbedPreferencesPage(String[] paths) {
		addAllPreferencePage(convertPathArrayToPreferencePageList(paths));
	}
	
	public TabbedPreferencesPage() {
		
	}
	
	public final XModel getPreferenceModel() {
		return ModelUtilities.getPreferenceModel();
	}

	private List convertPathArrayToPreferencePageList(String[] path) {
		
		XModel model  = getPreferenceModel();
		if(model==null) throw new IllegalArgumentException("Preference XModel not loaded."); //$NON-NLS-1$
		if(path==null) throw new IllegalArgumentException("String array with path's cannot be null"); //$NON-NLS-1$
		if(path.length==0) throw new IllegalArgumentException("Path array should contains at least one element.");  //$NON-NLS-1$
		List<XMOBasedPreferencesPage> pages = new ArrayList<XMOBasedPreferencesPage>();
		for (int i = 0; i < path.length; i++) {
			XModelObject o = model.getByPath(path[i]);
			XMOBasedPreferencesPage page = new XMOBasedPreferencesPage(o);
			pages.add(page);
			map.put(path[i], page); //map.put(o.getPathPart(), page);
		}
		return pages;
	}
	
	public XMOBasedPreferencesPage getXMOTabPage(String name) {
		return (XMOBasedPreferencesPage)map.get(name);
	}
	
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		this.noDefaultAndApplyButton();
		TabFolder tabbedComposite = new TabFolder(parent,SWT.NULL);
		tabbedComposite.setBackground(parent.getBackground());

		for (Iterator iter = pageList.iterator(); iter.hasNext();) {
			PreferencePage element = (PreferencePage) iter.next();
			TabItem newTab = new TabItem(tabbedComposite,SWT.NULL);
			element.createControl(tabbedComposite);
			if(element instanceof XMOBasedPreferencesPage)
				((XMOBasedPreferencesPage)element).initPageProperties();
			newTab.setControl(element.getControl());	
			newTab.setText(element.getTitle());
		}

		return tabbedComposite;
	}

	List<PreferencePage> pageList = new ArrayList<PreferencePage>();
	
	public void addPreferencePage(PreferencePage page) {
		pageList.add(page);
		if(page instanceof XMOBasedPreferencesPage) {
//			XMOBasedPreferencesPage xPage = (XMOBasedPreferencesPage)page;
			map.put(page.getTitle(),page);
		}
	}
	
	public void addAllPreferencePage(List page) {
		for (Iterator iter = page.iterator(); iter.hasNext();) {
			addPreferencePage((PreferencePage)iter.next());
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.IPreferencePage#performCancel()
	 */
	@Override
	public boolean performCancel() {
		boolean cancel = true;
		for (PreferencePage preferencePage : pageList) {
			cancel = preferencePage.performCancel() && cancel;
		}
		return cancel;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.IPreferencePage#performCancel()
	 */
	@Override
	public void performDefaults() {
		for (Iterator iter = pageList.iterator(); iter.hasNext();) {
			IPreferencePage element = (IPreferencePage) iter.next();
			if (element instanceof IPreferencePageExt) {
				IPreferencePageExt elementExt = (IPreferencePageExt) iter.next();
				elementExt.performDefaults();
			}
		}
		super.performDefaults();
	}

	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#performHelp()
	 */
	@Override
	public void performHelp() {
		super.performHelp();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		boolean isOk = true;
		for (PreferencePage page : pageList) {
			isOk = page.performOk() && isOk;
		}
		return isOk;
	}

	@Override
	public void dispose() {
		for (Iterator iter = pageList.iterator(); iter.hasNext();) {
			PreferencePage element = (PreferencePage) iter.next();
			element.dispose();
		}
		super.dispose();
	}
}
