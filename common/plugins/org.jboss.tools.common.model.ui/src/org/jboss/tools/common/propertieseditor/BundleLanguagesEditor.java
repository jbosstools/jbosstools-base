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
package org.jboss.tools.common.propertieseditor;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.bundle.CountriesHelper;
import org.jboss.tools.common.model.ui.ModelUIMessages;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarLayout;
import org.jboss.tools.common.model.ui.action.CommandBarListener;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.propertieseditor.bundlemodel.BundleModel;

public class BundleLanguagesEditor implements CommandBarListener, SelectionListener {
	static String CREATE = ModelUIMessages.BundleLanguagesEditor_Add;
	static String DELETE = ModelUIMessages.BundleLanguagesEditor_Delete;
	public Image IMAGE_DELETE = EclipseResourceUtil.getImage("images/actions/delete.gif"); //$NON-NLS-1$
	public Image IMAGE_EDIT = EclipseResourceUtil.getImage("images/actions/edit.gif"); //$NON-NLS-1$
	public Image IMAGE_CREATE = EclipseResourceUtil.getImage("images/actions/new.gif"); //$NON-NLS-1$
	BundleModel bundleModel;
	protected Composite control;
	protected ComboModel combomodel = new ComboModel();
	protected Combo combo;
	protected CommandBar bar = createBar();
	BundleLocaleEditor listener;
	
	public void dispose() {
		listener = null;
		if (combomodel!=null) combomodel.dispose();
		combomodel = null;
		if (bar!=null) bar.dispose();
		bar = null;
	}
	
	public void setBundleModel(BundleModel bundleModel) {
		this.bundleModel = bundleModel;
	}
	
	public void addListener(BundleLocaleEditor listener) {
		this.listener = listener;
	}
	
	public Control createControl(Composite parent) {
		control = new Composite(parent, SWT.NONE);
		control.setLayout(new GridLayout(2, false));
		createComboControl(control);
		bar.createControl(control);
		return control;	
	}
	
	private Control createComboControl(Composite parent) {
		Composite control = new Composite(parent, SWT.NONE);
		control.setLayout(new GridLayout(2, false));
		Label label = new Label(control, SWT.NONE);
		label.setText(ModelUIMessages.BundleLanguagesEditor_LanguageCountry);
		combo = new Combo(control, SWT.DROP_DOWN | SWT.READ_ONLY);
		combomodel.setCombo(combo);
		update();
		combo.addSelectionListener(this);
		return control;	
	}
	
	private CommandBar createBar() {
		CommandBar c = new CommandBar();
		CommandBarLayout layout = c.getLayout();
		layout.iconsOnly = true;
		layout.asToolBar = true;
		c.setCommands(new String[]{CREATE, DELETE});
		c.setEnabled(CREATE, true);
		c.setEnabled(DELETE, true);
		c.addCommandBarListener(this);
		c.setImage(CREATE, IMAGE_CREATE);
		c.setImage(DELETE, IMAGE_DELETE);
		return c;
	}

	public Control getControl() {
		return control;
	}
	
	boolean lock = false;
	
	public void update() {
		if(combo == null || combo.isDisposed()) return;
		if(lock) return;
		lock = true;
		String[] ls = bundleModel.getLocales();
		String lc = bundleModel.getCurrentLocale();
		if(isChanged(ls, combomodel)) setBoxValues(ls, combomodel, lc);
		else combomodel.refresh();
		control.getParent().update(); 
		control.redraw();
		lock = false;
	}
	
	private boolean isChanged(Object[] vs, ComboModel combomodel) {
		if(combomodel.getSize() != vs.length) return true;
		for (int i = 0; i < vs.length; i++)
		  if(vs[i] != combomodel.getElementAt(i)) return true;
		return false;
	}

	private Object setBoxValues(Object[] vs,ComboModel combomodel, Object selected) {
		boolean e = false;
		combomodel.removeAllElements();
		for (int i = 0; i < vs.length; i++) {
			if(vs[i] == selected) e = true;
			combomodel.addElement(vs[i]);
		}
		if(!e) selected = (vs.length > 0) ? vs[0] : null;
		if(selected != null) combomodel.setSelectedItem(selected);
		return selected;
	}

	public void action(String command) {
		if(CREATE.equals(command)) add();
		else if(DELETE.equals(command)) delete();
	}
	
	private void add() {
		CountriesHelper.init(bundleModel.getModelObject().getModel());
		XModelObject o = bundleModel.getModelObject().getModel().createModelObject("BundleList", null); //$NON-NLS-1$
		XActionInvoker.invoke("CreateActions.AddBundle", o, null); //$NON-NLS-1$
		XModelObject[] os = o.getChildren();
		if(os.length == 0) return;
		String lg = os[0].getAttributeValue("language"); //$NON-NLS-1$
		String ct = os[0].getAttributeValue("country"); //$NON-NLS-1$
		String locale = ((lg + ct).length() == 0) ? "" : lg + "_" + ct; //$NON-NLS-1$ //$NON-NLS-2$
		bundleModel.addLocale(locale);
		update();
		widgetSelected(null);
	}
	
	private void delete() {
		int i = combo.getSelectionIndex();
		Object o = combomodel.getSelectedItem();
		if(o == null) return;
		bundleModel.removeLocale(o.toString());
		String[] is = bundleModel.getLocales();
		if(is.length <= i) i = is.length - 1;
		if(i >= 0) bundleModel.setCurrentLocale(is[i]);
		update();
		widgetSelected(null);
	}
	
	public void widgetSelected(SelectionEvent e) {
		if(lock) return;
		Object o = combomodel.getSelectedItem();
		String locale = (o == null) ? "" : o.toString(); //$NON-NLS-1$
		bundleModel.setCurrentLocale(locale);
		if(listener != null) listener.update();
	}


	public void widgetDefaultSelected(SelectionEvent e) {}

	class ComboModel extends DefaultComboModel {
		public String getPresentation(Object object) {
			String s = super.getPresentation(object);
			return (s.length() == 0) ? "default" : s; //$NON-NLS-1$
		}		
	}
	
}

class DefaultComboModel {
	protected Combo combo;
	protected ArrayList<Object> list = new ArrayList<Object>();
	
	public DefaultComboModel() {}
	
	public void dispose() {
		if (list!=null) list.clear();
		list = null;
	}
	
	public void setCombo(Combo combo) {
		this.combo = combo;
	}
	
	public int getIndexOf(Object object) {
		for (int i = 0; i < list.size(); i++) if(list.get(i) == object) return i;
		return -1;  	
	}
	public void removeAllElements() {
		if(combo != null) combo.removeAll();
		list.clear(); 
	}
	public Object getSelectedItem() {
		return (combo == null) ? null : getElementAt(combo.getSelectionIndex());
	}
	public void setSelectedItem(Object o) {
		if(combo == null) return;
		int i = getIndexOf(o);
		if(i < 0) combo.setText(""); else combo.setText(combo.getItem(i)); //$NON-NLS-1$
	}
	public Object getElementAt(int i) {
		return (i < 0 || i >= list.size()) ? null : list.get(i);
	}
	public int getSize() {
		return list.size();
	}
	public void addElement(Object o) {
		list.add(o);
		if(combo != null) combo.add(getPresentation(o));			
	}
	
	public String getPresentation(Object object) {
		return (object == null) ? "" : object.toString(); //$NON-NLS-1$
	}
	
	public void refresh() {
		if(combo == null || isUpToDate()) return;
		Object selected = getSelectedItem();
		combo.removeAll();
		int s = getSize();
		for (int i = 0; i < s; i++) 
		  combo.add(getPresentation(getElementAt(i)));		
		if(selected != null) setSelectedItem(selected);
		combo.pack(true);
	}
	
	private boolean isUpToDate() {
		int s = getSize();
		if(combo.getItemCount() != s) return false;
		for (int i = 0; i < s; i++) {
			String s1 = getPresentation(getElementAt(i));
			String s2 = combo.getItem(i);
			if(s1 == null || !s1.equals(s2)) return false;
		}
		return true;
	}

}
