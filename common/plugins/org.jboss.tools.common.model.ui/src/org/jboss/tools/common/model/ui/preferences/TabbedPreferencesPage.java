/*
 * Created on Sep 1, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.ui.preferences;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.jboss.tools.common.model.ui.util.ModelUtilities;
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
import org.jboss.tools.common.model.util.ClassLoaderUtil;

/**
 * @author eskimo
 *
 */
public class TabbedPreferencesPage extends PreferencePage implements IWorkbenchPreferencePage {
	static {
		ClassLoaderUtil.init();
	}
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
		if(model==null) throw new NullPointerException("Preference XModel not loaded.");
		if(path==null) throw new NullPointerException("String array with path's cannot be null");
		if(path.length==0) throw new IllegalArgumentException("Path array should contains at least one element."); 
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
	public void init(IWorkbench workbench) {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
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
	public boolean performCancel() {
		for (Iterator iter = pageList.iterator(); iter.hasNext();) {
			PreferencePage element = (PreferencePage) iter.next();
			element.performCancel();
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.IPreferencePage#performCancel()
	 */
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
	public void performHelp() {
		super.performHelp();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
	 */
	public boolean performOk() {
		for (Iterator iter = pageList.iterator(); iter.hasNext();) {
			PreferencePage element = (PreferencePage) iter.next();
			element.performOk();
		}
		return true;
	}

	public void dispose() {
		for (Iterator iter = pageList.iterator(); iter.hasNext();) {
			PreferencePage element = (PreferencePage) iter.next();
			element.dispose();
		}
		super.dispose();
	}
}
