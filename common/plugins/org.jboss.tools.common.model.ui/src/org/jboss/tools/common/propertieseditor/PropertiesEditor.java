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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.events.IExpansionListener;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.XMenuInvoker;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.messages.UIMessages;
import org.jboss.tools.common.model.ui.objecteditor.XChildrenEditor;
import org.jboss.tools.common.model.ui.texteditors.TextActionHelper;
import org.jboss.tools.common.model.util.AbstractTableHelper;

public class PropertiesEditor extends XChildrenEditor implements ITextEditor, ITextOperationTarget {
	static final String ENT_PROPERTY = "Property"; //$NON-NLS-1$
	static final String ATTR_NAME = "name"; //$NON-NLS-1$
	static final String ATTR_VALUE = "value"; //$NON-NLS-1$
	static final String ATTR_ENABLED = "enabled"; //$NON-NLS-1$

	XModelObject property = PreferenceModelUtilities.getPreferenceModel().createModelObject(ENT_PROPERTY, null);
	XAttributeSupport nsupport = new XAttributeSupport(property, XEntityDataImpl.create(new String[][]{
			{ENT_PROPERTY, "yes"}, //$NON-NLS-1$
			{ATTR_NAME, "no"}, //$NON-NLS-1$
	}), true);
	XAttributeSupport vsupport = new XAttributeSupport(property, XEntityDataImpl.create(new String[][]{
			{ENT_PROPERTY, "yes"}, //$NON-NLS-1$
			{ATTR_VALUE, "no"} //$NON-NLS-1$
	}));
	Button fake = null;
	
	private Label statistics;
	Composite panel = null;
	private ArrayList<String> actionMapping = new ArrayList<String>();
	private Map<String,IAction> actions = new HashMap<String,IAction>();
	private IEditorInput input;
	IEditorSite site;
	FPTableHelper pHelper;
	
	private QualifiedName filterOpenedId = new QualifiedName("", "filterOpened"); //$NON-NLS-1$ //$NON-NLS-2$
	private QualifiedName nameFilterId = new QualifiedName("", "nameFilter"); //$NON-NLS-1$ //$NON-NLS-2$
	private QualifiedName valueFilterId = new QualifiedName("", "valueFilter"); //$NON-NLS-1$ //$NON-NLS-2$
	private QualifiedName isFilterExpressionId = new QualifiedName("", "isFilterExpression"); //$NON-NLS-1$ //$NON-NLS-2$
	private boolean filterOpened = false;
	private boolean isFilterExpression = false;
	
	
	public PropertiesEditor() {
		xtable.setMultiSelected();
		setMnemonicEnabled(true);
	}

	protected AbstractTableHelper createHelper() {
		return pHelper = new FPTableHelper(this);
	}

	protected int[] getColumnWidthHints() {
		return new int[]{10, 20};
	}
	
	protected boolean areUpDounActionsEnabled() {
		return true;
	}

	public Control createControl(Composite parent) {
		panel = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		panel.setLayout(layout);
	
		nsupport.getPropertyEditorAdapterByName(ATTR_NAME).setValue(pHelper.nameFilter);
		vsupport.getPropertyEditorAdapterByName(ATTR_VALUE).setValue(pHelper.valueFilter);
		
		ExpandableComposite g = new ExpandableComposite(panel, SWT.NONE);
		g.setText(UIMessages.PROPERTIES_EDITOR_FILTER);
		g.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		Composite g1 = new Composite(g, SWT.NONE);
		GridLayout l1 = new GridLayout(3, false);
		l1.horizontalSpacing = 10;
		l1.marginRight = 0;
		l1.marginHeight = 0;
		l1.verticalSpacing = 0;
		g1.setLayout(l1);
		g1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		g.setClient(g1);
		g.setExpanded(filterOpened);
		g.addExpansionListener(new IExpansionListener() {		
			public void expansionStateChanging(ExpansionEvent e) {
			}		
			public void expansionStateChanged(ExpansionEvent e) {
				filterOpened = e.getState();
				panel.update();
				panel.layout();
			}
		});

		Composite ng = new Composite(g1, SWT.NONE);
		GridLayout nl = new GridLayout(2, false);
		nl.marginRight = 5;
		nl.marginHeight = 0;
		ng.setLayout(nl);
		ng.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		nsupport.fillComposite(ng);
		nsupport.addPropertyChangeListener(pHelper);

		Composite vg = new Composite(g1, SWT.NONE);
		GridLayout vl = new GridLayout(2, false);
		vl.marginHeight = 0;
		vg.setLayout(vl);
		vg.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		vsupport.fillComposite(vg);
		vsupport.addPropertyChangeListener(pHelper);

		fake = new Button(g1, SWT.CHECK);
		fake.setText(UIMessages.PROPERTIES_EDITOR_EXPRESSION);
		GridData fd = new GridData();
		fd.widthHint = convertHorizontalDLUsToPixels(g1, IDialogConstants.BUTTON_WIDTH);
		fake.setLayoutData(fd);
		fake.setSelection(isFilterExpression);
		fake.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				isFilterExpression = fake.getSelection();
				pHelper.applyFilters();
				refresh();				
			}		
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
//		fake.setVisible(false);

		statistics = new Label(panel, SWT.NONE);
		statistics.setVisible(false);
		GridData d = new GridData(GridData.FILL_HORIZONTAL);
		d.horizontalSpan = 2;
		d.heightHint = 1;
		statistics.setLayoutData(d);
		
		Control c = super.createControl(panel);
		c.setLayoutData(new GridData(GridData.FILL_BOTH));
	
		TMenuInvoker menu = new TMenuInvoker();
		menu.setViewer(xtable.getViewer()); 
		xtable.getViewer().getTable().addMouseListener(menu);
		getControl().addMouseListener(menu);
		
		return panel;	
	}
	
	protected void setMargins(CommandBar bar) {
		bar.getLayout().setMargins(10,10,0,10);
	}

	protected Color getItemColor(int i) {
		XModelObject o = helper.getModelObject(i);
		boolean disabled = "no".equals(o.getAttributeValue(ATTR_ENABLED)); //$NON-NLS-1$
		return ((disabled) ? GREYED_COLOR : DEFAULT_COLOR);
	}

	protected String getAddActionPath() {
		return "CreateActions.CreateProperty"; //$NON-NLS-1$
	}

	protected void edit() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o != null) callAction(o, "Properties.Edit"); //$NON-NLS-1$
	}
	
	public class TMenuInvoker extends XMenuInvoker {

		public XModelObject getSelectedModelObject() {
			int i = ((TableViewer)viewer).getTable().getSelectionIndex();
			if(i < 0) return null;
			return helper.getModelObject(i);
		}
	
	}

	///ITextEditor
	public IDocumentProvider getDocumentProvider() {
		return null;
	}

	public void close(boolean save) {}

	public boolean isEditable() {
		return false;
	}

	public void doRevertToSaved() {}

	public void setAction(String actionID, IAction action) {
		actions.put(actionID, action);
	}

	public IAction getAction(String actionId) {
		return actions.get(actionId);
	}

	public void setActionActivationCode(String actionId, char activationCharacter, int activationKeyCode, int activationStateMask) {
	}

	public void removeActionActivationCode(String actionId) {}

	public boolean showsHighlightRangeOnly() {
		return false;
	}

	public void showHighlightRangeOnly(boolean showHighlightRangeOnly) {}

	public void setHighlightRange(int offset, int length, boolean moveCursor) {}

	public IRegion getHighlightRange() {
		return null;
	}

	public void resetHighlightRange() {}

	public void selectAndReveal(int offset, int length) {}

	public IEditorInput getEditorInput() {
		return input;
	}

	public IEditorSite getEditorSite() {
		return site;
	}

	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		this.site = site;
		this.input = input;
		if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			loadPreferences(f);
		}
	}

	void loadPreferences(IFile f) {
		if(f == null || !f.exists()) return;
		try {
			filterOpened = "true".equals(f.getPersistentProperty(filterOpenedId)); //$NON-NLS-1$
			pHelper.nameFilter = f.getPersistentProperty(nameFilterId);
			pHelper.valueFilter = f.getPersistentProperty(valueFilterId);
			if(pHelper.nameFilter == null) pHelper.nameFilter = ""; //$NON-NLS-1$
			if(pHelper.valueFilter == null) pHelper.valueFilter = ""; //$NON-NLS-1$
			isFilterExpression = "true".equals(f.getPersistentProperty(isFilterExpressionId)); //$NON-NLS-1$
		} catch (CoreException e) {
			//ignore
		}
	}
	
	void savePreferences(IFile f) {
		if(f == null || !f.exists()) return;
		try {
			f.setPersistentProperty(filterOpenedId, "" + filterOpened); //$NON-NLS-1$
			f.setPersistentProperty(nameFilterId, pHelper.nameFilter);
			f.setPersistentProperty(valueFilterId, "" + pHelper.valueFilter); //$NON-NLS-1$
			f.setPersistentProperty(isFilterExpressionId, "" + isFilterExpression); //$NON-NLS-1$
		} catch (CoreException e) {
			//ignore
		}
	}

	public void addPropertyListener(IPropertyListener listener) {}

	public void createPartControl(Composite parent) {
		createControl(parent);
		createActions();
	}

	public IWorkbenchPartSite getSite() {
		return site;
	}

	public String getTitle() {
		return null;
	}

	public Image getTitleImage() {
		return null;
	}

	public String getTitleToolTip() {
		return null;
	}

	public void removePropertyListener(IPropertyListener listener) {}

	public void setFocus() {
		if(xtable == null || !xtable.isActive()) return;
		xtable.getTable().setFocus();
	}

	public Object getAdapter(Class adapter) {
		if (ITextOperationTarget.class.equals(adapter))	return this;
		return null;
	}

	public void doSave(IProgressMonitor monitor) {}

	public void doSaveAs() {}

	public boolean isDirty() {
		return false;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public boolean isSaveOnCloseNeeded() {
		return false;
	}	
	
	public boolean canDoOperation(int operation) {
//		Collection actions = this.actions.values();
//		Iterator i = actions.iterator();
//		IAction action;
//		while (i.hasNext()) {
//			action = (IAction)i.next();
//		}
		return true;
	}

	public void doOperation(int operation) {
		if (operation>actionMapping.size()) {
			ModelUIPlugin.getPluginLog().logError(new IllegalArgumentException("Can not find global action with index: "+operation)); //$NON-NLS-1$
		} else {
			String globalAction = (String)actionMapping.get(operation);
			doGlobalAction(globalAction);
		}
	}
	
	protected void createActions() {
		actionMapping.add(null);
		actionMapping.add(ITextOperationTarget.UNDO, ActionFactory.UNDO.getId());
		actionMapping.add(ITextOperationTarget.REDO, ActionFactory.REDO.getId());
		actionMapping.add(ITextOperationTarget.CUT, ActionFactory.CUT.getId());
		actionMapping.add(ITextOperationTarget.COPY, ActionFactory.COPY.getId());
		actionMapping.add(ITextOperationTarget.PASTE, ActionFactory.PASTE.getId());
		actionMapping.add(ITextOperationTarget.DELETE, ActionFactory.DELETE.getId());
		actionMapping.add(ITextOperationTarget.SELECT_ALL, ActionFactory.SELECT_ALL.getId());
		actionMapping.add(ITextOperationTarget.SHIFT_RIGHT, ITextEditorActionConstants.SHIFT_RIGHT);
		actionMapping.add(ITextOperationTarget.SHIFT_LEFT, ITextEditorActionConstants.SHIFT_LEFT);
		actionMapping.add(ITextOperationTarget.PRINT, ActionFactory.PRINT.getId());

		TextActionHelper.addCutAction(this);
		TextActionHelper.addCopyAction(this);
		TextActionHelper.addPasteAction(this);
		TextActionHelper.addDeleteAction(this);

	}

	public void doGlobalAction(String actionName) {
		if(ITextEditorActionConstants.DELETE.equals(actionName)) {
			action(XChildrenEditor.DELETE);
		} else if(ITextEditorActionConstants.COPY.equals(actionName)) {
			doXActionCopy();
		} else if(ITextEditorActionConstants.PASTE.equals(actionName)) { 
			doXActionPaste();
		}
	}
	
	private void doXActionCopy() {
		ISelection selection = getSelectionProvider().getSelection();
		if(selection == null || selection.isEmpty() || !(selection instanceof StructuredSelection)) return;
		StructuredSelection ss = (StructuredSelection)selection;
		if(!(ss.getFirstElement() instanceof XModelObject)) return;
		XModelObject object = (XModelObject)ss.getFirstElement();
		XModelObject[] os = null;
		if(ss.size() > 1) {
			os = new XModelObject[ss.size()];
			Iterator it = ss.iterator();
			for (int i = 0; i < os.length; i++) os[i] = (XModelObject)it.next(); 
		}
		invokeXAction(XAction.COPY, object, os);
	}

	private void doXActionPaste() {
		invokeXAction(XAction.PASTE, helper.getModelObject(), null);
	}

	private void invokeXAction(String actionPath, XModelObject object, XModelObject[] os) {
		XAction action = XActionInvoker.getAction(actionPath, object);
		if(action == null) return;
		if(os == null) {
			if(!action.isEnabled(object)) return;
			XActionInvoker.invoke(actionPath, object, new Properties());
		} else {
			if(!action.isEnabled(object, os)) return;
			XActionInvoker.invoke(actionPath, object, os, new Properties());
		}
	}

	public void refresh() {
		xtable.update();
		updateBar();
		validateStatistics();
	}
	
	void validateStatistics() {
		int filtered = pHelper.filteredChildren.length;
		int total = pHelper.getModelObject().getChildren().length;
		boolean errors = pHelper.compileError.length() > 0;
		boolean visible = (filtered != total || errors);
		boolean mod = visible != statistics.isVisible();
		if(mod) {
			statistics.setVisible(visible);
			GridData d = (GridData)statistics.getLayoutData();
			d.heightHint = visible ? SWT.DEFAULT : 1;
		}
		if(filtered != total) {
			statistics.setText(NLS.bind(UIMessages.PROPERTIES_EDITOR_FILTER_MATCHES, filtered, total));
		} else if(errors) {
			statistics.setText(pHelper.compileError);
		}
		statistics.update();
		panel.update();
		panel.layout();
		panel.getParent().update();
		panel.getParent().layout();
	}

	public void dispose() {
		super.dispose();
		if(input instanceof IFileEditorInput) {
			IFile f = ((IFileEditorInput)input).getFile();
			savePreferences(f);
		}
	}
}

class FPTableHelper extends AbstractTableHelper implements PropertyChangeListener {
	PropertiesEditor pe;
	String nameFilter = ""; //$NON-NLS-1$
	String valueFilter = ""; //$NON-NLS-1$
	String compileError = ""; //$NON-NLS-1$

	XModelObject[] filteredChildren = new XModelObject[0];
	long ts = -1;

	public void setModelObject(XModelObject object) {
		super.setModelObject(object);
	}

	void applyFilters() {
		ts = object == null ? -1 : object.getTimeStamp();
		compileError = ""; //$NON-NLS-1$
		if(object == null) {
			filteredChildren = new XModelObject[0];
		} else if(nameFilter.length() == 0 && valueFilter.length() == 0) {
			filteredChildren = object.getChildren();
		} else if(pe.fake.getSelection()) {
			filteredChildren = object.getChildren();
			Pattern pn = null, pv = null;
			if(nameFilter.length() > 0) try {
				pn = Pattern.compile(nameFilter);
			} catch (Exception e) {
				compileError = NLS.bind(UIMessages.PROPERTIES_EDITOR_ILLEGAL_NAME_EXPRESSION, e.getMessage());
				return;
			}
			if(valueFilter.length() > 0) try {
				pv = Pattern.compile(valueFilter);
			} catch (Exception e) {
				compileError = NLS.bind(UIMessages.PROPERTIES_EDITOR_ILLEGAL_VALUE_EXPRESSION, e.getMessage());
				return;
			}
			XModelObject[] children = object.getChildren();
			List<XModelObject> list = new ArrayList<XModelObject>();
			for (XModelObject c: children) {
				String n = c.getAttributeValue(PropertiesEditor.ATTR_NAME);
				String v = c.getAttributeValue(PropertiesEditor.ATTR_VALUE);
				if(pn != null && !pn.matcher(n).find()) {
					continue;
				}
				if(pv != null && !pv.matcher(v).find()) {
					continue;
				}
				
				list.add(c);
			}
			filteredChildren = list.toArray(new XModelObject[0]);
		} else {
			String lowNameFilter = nameFilter.toLowerCase();
			String lowValueFilter = valueFilter.toLowerCase();
			XModelObject[] children = object.getChildren();
			List<XModelObject> list = new ArrayList<XModelObject>();
			for (XModelObject c: children) {
				String n = c.getAttributeValue(PropertiesEditor.ATTR_NAME);
				String v = c.getAttributeValue(PropertiesEditor.ATTR_VALUE);
				if(nameFilter.length() > 0) {
					if(n.toLowerCase().indexOf(lowNameFilter) < 0) continue; //TODO improve
				}
				if(valueFilter.length() > 0) {
					if(v.toLowerCase().indexOf(lowValueFilter) < 0) continue; //TODO improve
				}
				
				list.add(c);
			}
			filteredChildren = list.toArray(new XModelObject[0]);
		}
	}
	public FPTableHelper(PropertiesEditor pe) {
		this.pe = pe;
	}

    public int size() {
    	if(object == null) return 0;
    	if(ts != object.getTimeStamp()) {
    		applyFilters(); 
    	}
        return filteredChildren.length;
    }

    public XModelObject getModelObject(int r) {
        if(object == null) return null;
    	if(ts != object.getTimeStamp()) {
    		applyFilters(); 
    	}
        XModelObject[] cs = filteredChildren;
        return (r < 0 || r >= cs.length) ? null : cs[r];
    }


	public String[] getHeader() {
		return new String[]{"name", "value"}; //$NON-NLS-1$ //$NON-NLS-2$
	}

	public void propertyChange(PropertyChangeEvent evt) {
		if(evt.getSource() instanceof DefaultValueAdapter) {
			DefaultValueAdapter a = (DefaultValueAdapter)evt.getSource();
			String name = a.getAttribute().getName();
			if(PropertiesEditor.ATTR_NAME.equals(name)) {
				nameFilter = "" + evt.getNewValue(); //$NON-NLS-1$
			} else if(PropertiesEditor.ATTR_VALUE.equals(name)) {
				valueFilter = "" + evt.getNewValue(); //$NON-NLS-1$
			}
			applyFilters();
			pe.refresh();
		}
		
	}
}
