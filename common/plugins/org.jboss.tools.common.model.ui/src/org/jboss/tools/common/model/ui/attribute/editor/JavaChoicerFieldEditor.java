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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.beans.PropertyChangeEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueEditor;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.IPropertyDescriptorEx;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.AccessibleClasses;
//import org.jboss.tools.common.model.util.ClassLoaderUtil;
import org.jboss.tools.common.model.util.AccessibleJava;
import org.jboss.tools.common.model.util.ISimpleTree;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.ModelImages;
import org.jboss.tools.common.model.util.XModelObjectUtil;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class JavaChoicerFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, IValueEditor, ModifyListener, ISelectionChangedListener {
	protected IPropertyEditor propertyEditor;
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;
	protected IPropertyDescriptorEx description;
	private TreeViewer tree;
	private Text text;
	private String stringValue;	
	private Composite treeAndText;
	private XModel model;
	
	private JavaElement rootElement = new JavaRootElement("Java");
	AccessibleJava.IContextPackageProvider cpp = null;
	
	private static final String ROOT_PATH = "%root%";
	private static final String EMPTY_PATH = "";
	private static final Image ROOT_IMAGE = ModelImages.getImage("images/navigationtree/java.gif");
	private static final Image PACKAGE_IMAGE = JavaPluginImages.get(JavaPluginImages.IMG_OBJS_PACKAGE);
	private static final Image CLASS_IMAGE = JavaPluginImages.get(JavaPluginImages.IMG_OBJS_CLASS);
	
	public JavaChoicerFieldEditor() {}
	
	public JavaChoicerFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	public JavaChoicerFieldEditor(String name, String labelText, Composite parent) {
		init(name, labelText);
		createControl(parent);
	}

	protected void adjustForNumColumns(int numColumns) {
		Control control = getLabelComposite();
		((GridData)control.getLayoutData()).horizontalSpan = numColumns;
		((GridData)treeAndText.getLayoutData()).horizontalSpan = numColumns - 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		/*
		Control control = createTabbedPane(parent);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		//gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
		gd.verticalAlignment = GridData.GRAB_VERTICAL;
		control.setLayoutData(gd);
		*/
		
		Control control = getLabelComposite(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		//gd.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
		gd.verticalAlignment = GridData.GRAB_VERTICAL;
		control.setLayoutData(gd);

		treeAndText = getTreeAndTextControl(parent);
		gd = new GridData(GridData.FILL_BOTH);
		//gd.verticalAlignment = GridData.FILL;
		gd.horizontalSpan = numColumns;
		//gd.horizontalSpan = numColumns - 1;
		gd.grabExcessHorizontalSpace = true;
		treeAndText.setLayoutData(gd);
		
	}

	protected void doLoad() {
	}

	protected void doLoadDefault() {
	}

	protected void doStore() {
	}

	public int getNumberOfControls() {
		return 2;
	}

	protected Control createFavoritesChooser(Composite parent) {
		return getTreeAndTextControl(parent);
	}

	protected Composite getTreeAndTextControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		composite.setLayout(layout);
		layout.horizontalSpacing = 7;
		layout.verticalSpacing = 7;
		layout.marginHeight = 10;
		layout.marginWidth = 10;
		
		GridData gd;
		Label label;
		
		label = new Label(composite, SWT.NONE);
		label.setText(EditorMessages.getString("JavaChoicerFieldEditor.Tab1.Tree.Label"));
		
		tree = new TreeViewer(composite,SWT.BORDER | SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL);
		gd = new GridData(GridData.FILL_BOTH);
		//gd.verticalAlignment = GridData.FILL_VERTICAL;
		tree.getControl().setLayoutData(gd);
		tree.setContentProvider(new ContentProviderEx());
		tree.setLabelProvider(new LabelProviderEx());
		tree.setInput("Java");
		if ((this.stringValue!=null)&&(this.stringValue.length()>0)) {
			ISelection selection = getSelection(this.stringValue);
			tree.setSelection(selection); 		
		}
		tree.addSelectionChangedListener(this);
		tree.setAutoExpandLevel(2);

		label = new Label(composite, SWT.NONE);
		label.setText(EditorMessages.getString("JavaChoicerFieldEditor.Tab1.Text.Label"));

		text = new Text(composite,SWT.SINGLE | SWT.BORDER);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		text.setLayoutData(gd);
		text.setText(valueProvider.getStringValue(true));
		try {
			text.setSelection(valueProvider.getStringValue(true).length());
		} catch (Exception e) {
			//ignore
		}
		text.addModifyListener(this);
		text.setFocus();
		return composite;
	}
	
	private Object getObject(String path) {
		StringTokenizer st = new StringTokenizer(path, ".");
		JavaElement element = rootElement;
		String find = "";
		while (st.hasMoreTokens()) {
			find = st.nextToken();
			find = (element.getPath().equals(rootElement.getPath()))?find:element.getPath()+"."+find;
			if (element.getChildByPath(find) == null) {
				// try find like element startWith()
				int childCount = element.getChildCount();
				for (int i = 0; i < childCount; ++i) {
					if (element.getChildAt(i).getPath().startsWith(find)) {
						return element.getChildAt(i); 
					}
				}
				return null;//new StructuredSelection(new Object[] {element});
			}
			element = element.getChildByPath(find);
		}
		return element;
	}
	private ISelection getSelection(String path) {
		Object o = getObject(path);
		if(o == null && cpp != null && path != null && path.length() > 0 && path.indexOf('.') < 0) {
			String pkg = cpp.getContextPackage();
			if(pkg != null && pkg.length() > 0) {
				o = getObject(pkg + "." + path);
			}
		}
		return new StructuredSelection(new Object[] {o});
	}
	
	
	protected String oldValue = null;

	protected void valueChanged(String newValue) {
		if ((newValue != null) && (!newValue.equals(oldValue))) {
			text.removeModifyListener(this);
			if ((text.getText()!=null)&&(!text.getText().equals(newValue))) {
				text.setText(newValue);
				try {
					text.setSelection(newValue.length());
				} catch (Exception e) {
					ModelUIPlugin.getPluginLog().logError(e);
				}
			}
			PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
			valueChangeListener.valueChange(event);
			oldValue = newValue;
			this.stringValue = newValue;
			text.addModifyListener(this);
		}
	}


	protected void handleMouseUp(MouseEvent e) {
	}

	ISimpleTree list = null;

	class ContentProviderEx implements ITreeContentProvider {
		
		public ContentProviderEx() {
///			if(model == null) throw new RuntimeException("Model is not provided for java class chooser.");
			list = new AccessibleClasses(model, true);
		}
		
		public Object[] getChildren(Object parentElement) {
			return ((JavaElement)parentElement).getChildrens();
		}
		public Object getParent(Object element) {
			if (element instanceof JavaElement) {
				return ((JavaElement)element).getParent();
			} else {
				return null;
			}
		}
		public boolean hasChildren(Object element) {
			return ((JavaElement)element).getChildCount()>0;
		}

		public Object[] getElements(Object inputElement) {
			rootElement.check();
			return rootElement.getChildrens();
		}

		public void dispose() {
			list = null;
		}
		
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}
	} 
	class LabelProviderEx extends LabelProvider {

		public Image getImage(Object element) {
			JavaElement javaElement = (JavaElement)element;
			if (ROOT_PATH.equals(javaElement.getPath())) {
				return ROOT_IMAGE;
			} else if (javaElement.isPackage()) {
				return PACKAGE_IMAGE;
			} else {
				return CLASS_IMAGE;
			}
		}

		public String getText(Object element) {
			return super.getText(element);
		}
	}
	class JavaRootElement extends JavaElement {
		public JavaRootElement(String name) {
			path = ROOT_PATH;
			this.name = name;
		}

		protected String _getPath() {
			return null;
		}
	}
	class JavaElement {
		protected JavaElement parent = null;
		private boolean isLoaded = false;
		protected String path = "";
		protected String name = "";
		protected boolean isPackage = false;
		private List<JavaElement> children = new ArrayList<JavaElement>();

		protected JavaElement() {
		}

		public JavaElement(String parentpath, String name) {
			this.path = (parentpath == null) ? name :  parentpath + "." + name;
			this.name = name;
		}

		public boolean isPackage() {
			return isPackage;
		}

		public JavaElement createChild(String name) {
			JavaElement c = new JavaElement(_getPath(), name);
			c.parent = this;
			return c;
		}
		
		public Object[] getChildrens() {
			return children.toArray(new Object[children.size()]);
		}

		public String getPath() {
			return path;
		}

		protected String _getPath() {
			return path;
		}
		
		public JavaElement getChildByPath(String path) {
			check();
			JavaElement element;
			for (int i = 0; i < children.size(); ++i) {
				element = children.get(i);
				if (element.getPath().equals(path)) {
					return element;
				}
			}
			return null;
		}

		protected void check() {
			if(isLoaded) return;
			isLoaded = true;
			String[] ns = list.getContent(path);
			if(ns == null) return;
			for (int i = 0; i < ns.length; i++) {
				String s = ns[i];
				boolean p = s.endsWith(".");
				if(p) s = s.substring(0, s.length() - 1);
				if(!isJavaName(s)) continue;
				JavaElement c = createChild(s);
				c.isPackage = p;
				children.add(c);
			}
		}

	    boolean isJavaName(String name) {
	    	if(name.length() == 0) return false;
	    	if(!Character.isJavaIdentifierStart(name.charAt(0))) return false;
	    	for (int i = 1; i < name.length(); i++) {
	        	if(!Character.isJavaIdentifierPart(name.charAt(i))) return false;
	    	}
	    	return true;
	    }

		public JavaElement getChildAt(int childIndex) {
			check();
			return children.get(childIndex);
		}

		public int getChildCount() {
			check();
			return children.size();
		}

		public JavaElement getParent() {
			return parent;
		}

		public int getIndex(JavaElement node) {
			check();
			for (int i = 0; i < children.size(); i++)
				if(children.get(i) == node) return i;
			return -1;
		}

		public boolean getAllowsChildren() {
			return true;
		}

		public boolean isLeaf() {
			check();
			return getChildCount() == 0;
		}

		public Iterator children() {
			return children.iterator();
		}

		public String toString() {
			return name;
		}
	} 

	// IValueEditor
	public void setValueChangeListener(IValueChangeListener valueChangeListener) {
		this.valueChangeListener = valueChangeListener;
	}
	public void setValueProvider(IValueProvider valueProvider) {
		this.valueProvider = valueProvider;
	}

	// ModifyListener
	public void modifyText(ModifyEvent e) {
		String newValue = ((Text)e.getSource()).getText(); 
		tree.removeSelectionChangedListener(this);
		if (newValue.endsWith(" ")) {
			StructuredSelection selection = (StructuredSelection)tree.getSelection();
			JavaElement element = (JavaElement)selection.getFirstElement();
			if (element != null) {
				if (!ROOT_PATH.equals(element.getPath())) {
					valueChanged(element.getPath());
				} else {
					valueChanged(EMPTY_PATH);
				}
				newValue = stringValue;
				if(newValue == null) newValue = "";
				text.setText(newValue);
				try {
					text.setSelection(newValue.length());
				} catch(Exception exc) {
					//ignore
				}
			}
		} else {
			valueChanged(newValue.trim());
		}
		tree.setSelection(getSelection(((Text)e.getSource()).getText()));
		Object currentObject = getObject(((Text)e.getSource()).getText());
		if (currentObject!=null) { 
			tree.expandToLevel(currentObject,1);
		}
		tree.addSelectionChangedListener(this);
	}
	
	protected void init() {
		this.stringValue = valueProvider.getStringValue(true);
		//setPropertyChangeListener(this);
	}

	public void setEnabled(boolean enabled){
		// TODO;
		super.setEnabled(enabled);
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			Object input = propertyEditor.getInput();
			if(input instanceof DefaultValueAdapter) {
				model = ((DefaultValueAdapter)input).getModel();
				XModelObject xmo = ((DefaultValueAdapter)valueProvider).getModelObject();
				XAttribute attr = ((DefaultValueAdapter)valueProvider).getAttribute();
				if (attr!=null) {
					
					AccessibleJava.IContextPackageProvider cpp = getPackageProvider();
					if(cpp != null) {
						this.cpp = cpp;
						cpp.setObject(xmo);
					}
					
				}
			}
		}
		init();
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getTreeAndTextControl(parent)};
		//return new Control[] {createTabbedPane(parent)};
	}

	public void selectionChanged(SelectionChangedEvent event) {
		StructuredSelection selection = (StructuredSelection)event.getSelection();
		JavaElement element = selection.isEmpty() ? null : (JavaElement)selection.getFirstElement();
		if (element != null && !ROOT_PATH.equals(element.getPath())) {
			valueChanged(element.getPath());
		} else {
			valueChanged(EMPTY_PATH);
		}
	}

	public void cut() {
	}

	public void copy() {
	}

	public void paste() {
	}

	public void delete() {
	} 

	private AccessibleJava.IContextPackageProvider getPackageProvider() {
		IModelPropertyEditorAdapter adapter = (IModelPropertyEditorAdapter)propertyEditor.getInput();
		XAttribute a = adapter.getAttribute();
		if(a == null) return null;
		String cls = a.getProperty("contextPackageProvider");
		if(cls == null || cls.length() == 0) return null;
		try {
			AccessibleJava.IContextPackageProvider o = (AccessibleJava.IContextPackageProvider)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
			if(o != null) o.setObject(((DefaultValueAdapter)adapter).getModelObject());
			return o;
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}		
		return null;
	}
}
