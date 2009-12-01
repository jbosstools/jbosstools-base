/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.editor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.JavaElementLabelProvider;
import org.eclipse.jdt.ui.StandardJavaElementContentProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.jst.j2ee.internal.plugin.J2EEUIMessages;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.internal.util.Util;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.ui.IValidator;
import org.jboss.tools.common.ui.ValidatorFactory;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;
import org.jboss.tools.common.ui.wizard.IParameter;

/**
 * 
 * @author eskimo
 *
 */
public class SwtFieldEditorFactory implements IFieldEditorFactory {

	/**
	 * 
	 */
	public IFieldEditor createCheckboxEditor(String name, String label,
			boolean defaultValue) {
		CompositeEditor editor = new CompositeEditor(name,label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new CheckBoxFieldEditor(name,label,Boolean.valueOf(defaultValue))});
		return editor;
	}

	/*
	 * Starting from 4022 revision it creates standart combo box, if it be necessary
	 * to use custom combo box, use another implementation of this method.
	 * PS. custom combo box looks ugly under mac os. 
	 */
	public ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue) {
		TaggedComboFieldEditor editor = new TaggedComboFieldEditor(name,label,values, defaultValue,false);
		return editor;
	}

	/**
	 * 
	 */
	public ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue, boolean editable) {
		TaggedComboFieldEditor editor = new TaggedComboFieldEditor(name,label,values, defaultValue,editable);
		return editor;
	}

	public ITaggedFieldEditor createRadioEditor(String name, String label,
			List<String> labels, List values, Object defaultValue) {
		TaggedRadioFieldEditor editor = new TaggedRadioFieldEditor(name,label, labels, values, defaultValue);
		return editor;
	}

	/**
	 * 
	 */
	public IFieldEditor createTextEditor(String name, String label, String defaultValue) {
		CompositeEditor editor = new CompositeEditor(name,label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new TextFieldEditor(name,label, defaultValue)});
		return editor;
	}

	/**
	 * 
	 */
	public IFieldEditor createUneditableTextEditor(String name, String label, String defaultValue) {
		CompositeEditor editor = new CompositeEditor(name, label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new TextFieldEditor(name,label, defaultValue,false)});
		return editor;
	}

	/**
	 * 
	 */
	public IFieldEditor createBrowseFolderEditor(String name, String label, String defaultValue) {
		CompositeEditor editor = new CompositeEditor(name, label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new TextFieldEditor(name,label, defaultValue),
				new ButtonFieldEditor(name,createSelectFolderAction(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_BROWS),defaultValue)});
		return editor;
	}

	/**
	 * 
	 */
	public IFieldEditor createBrowseWorkspaceFolderEditor(String name, String label, String defaultValue) {
		ButtonFieldEditor.ButtonPressedAction action = createSelectWorkspaceFolderAction(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_BROWS, defaultValue);
		CompositeEditor editor = new CompositeEditor(name, label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new TextFieldEditor(name,label, defaultValue),
				new ButtonFieldEditor(name, action, defaultValue)});
		action.setFieldEditor(editor);
		return editor;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.IFieldEditorFactory#createBrowseSourceFolderEditor(java.lang.String, java.lang.String, java.lang.String)
	 */
	public IFieldEditor createBrowseSourceFolderEditor(String name,	String label, String defaultValue) {
		ButtonFieldEditor.ButtonPressedAction action = createSelectSourceFolderAction(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_BROWS, defaultValue);
		CompositeEditor editor = new CompositeEditor(name, label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name, label),
				new TextFieldEditor(name, label, defaultValue),
				new ButtonFieldEditor(name, action, defaultValue)});
		action.setFieldEditor(editor);
		return editor;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.IFieldEditorFactory#createBrowsePackageEditor(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	public IFieldEditor createBrowsePackageEditor(String name,	String label, String initSourceFolderPath, String defaultValue) {
		ButtonFieldEditor.ButtonPressedAction action = new SelectJavaPackageAction(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_BROWS, initSourceFolderPath, defaultValue);
		IFieldEditor editor = createButtonFieldEditor(
				name, label, defaultValue, 
				action, ValidatorFactory.NO_ERRORS_VALIDATOR);
		return editor;
	}

	/**
	 * @param buttonName
	 * @return
	 */
	public ButtonFieldEditor.ButtonPressedAction createSelectSourceFolderAction(String buttonName, final String initPath) {
		ButtonFieldEditor.ButtonPressedAction action = new ButtonFieldEditor.ButtonPressedAction(buttonName) {
			private String inerInitPath;

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.action.Action#run()
			 */
			@Override
			public void run() {
				final ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(
						Display.getCurrent().getActiveShell(),
						new JavaElementLabelProvider(), new JavaSourceContentProvider());
				dialog.setInput(ResourcesPlugin.getWorkspace());
				String path = inerInitPath!=null?inerInitPath:initPath;
				IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
				if (resource!=null) {
					IProject project = resource.getProject();
					if (project!=null && project.isOpen()) {
						IJavaProject javaProject = EclipseUtil.getJavaProject(project);
						try {
							IPackageFragmentRoot[] roots = javaProject.getPackageFragmentRoots();
							for (int i= 0; i < roots.length; i++) {
								if (roots[i].getKind() == IPackageFragmentRoot.K_SOURCE && roots[i].getResource().equals(resource)) {
									dialog.setInitialSelection(roots[i]);
									break;
								}
							}
						} catch (JavaModelException e) {
							CommonUIPlugin.getDefault().logError(e);
						}
					}
				}
				dialog.setValidator(new ISelectionStatusValidator(){
					public IStatus validate(Object[] selection) {
						if(selection.length>0) {
							if(selection[0] instanceof IPackageFragmentRoot) {
								return new Status(IStatus.OK, CommonUIPlugin.PLUGIN_ID, IStatus.OK, null, null);
							}
						}
						return new Status(IStatus.ERROR, CommonUIPlugin.PLUGIN_ID, IStatus.ERROR, null, null);
					}
				});
				dialog.setAllowMultiple(false);
				dialog.setTitle(CommonUIMessages.SELECT_WORKSPACE_FOLDER_DIALOG_TITLE); 
				dialog.setMessage(CommonUIMessages.SELECT_WORKSPACE_FOLDER_DIALOG_MESSAGE); 
				if (dialog.open() == Window.OK) {
					IPackageFragmentRoot pack = (IPackageFragmentRoot) dialog.getFirstResult();
					IPath newPath = pack.getResource().getFullPath();
					String value = newPath.toString();
					inerInitPath = value;
					getFieldEditor().setValue(value);
				}
			}
		};
		return action;
	}

	/**
	 * @author Alexey Kazakov
	 */
	private static class JavaSourceContentProvider extends StandardJavaElementContentProvider {

		boolean providePackages = false;
		
		public JavaSourceContentProvider() {
			super(false);
		}

		public JavaSourceContentProvider(boolean providePackages) {
			this();
			this.providePackages = providePackages;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jdt.ui.StandardJavaElementContentProvider#hasChildren(java.lang.Object)
		 */
		@Override
		public boolean hasChildren(Object element) {
			if (element instanceof IPackageFragmentRoot) {
				return providePackages;
			} else if(element instanceof IPackageFragment) {
				return false;
			}
			return true;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jdt.ui.StandardJavaElementContentProvider#getChildren(java.lang.Object)
		 */
		@Override
		public Object[] getChildren(Object element) {
			if (!exists(element)) {
				return NO_CHILDREN;
			}

			if (element instanceof IWorkspace) {
		        IWorkbenchAdapter adapter = (IWorkbenchAdapter)Util.getAdapter(element, IWorkbenchAdapter.class);
		        if (adapter != null) {
		        	Object[] children = adapter.getChildren(element);
		        	List<Object> result = new ArrayList<Object>();
		        	for (int i = 0; i < children.length; i++) {
		        		if(children[i] instanceof IProject) {
		        			if(EclipseUtil.getJavaProject((IProject)children[i])!=null) {
		        				result.add(children[i]);
		        			}
		        		}
					}
		            return result.toArray(new Object[]{});
		        }
		        return new Object[0];
			}

			if (element instanceof IProject) {
				element = EclipseUtil.getJavaProject((IProject)element);
				if(element==null) {
					return NO_CHILDREN;
				}
			}

			if (element instanceof IJavaProject) {
				try {
					return getPackageFragmentRoots((IJavaProject)element);
				} catch (JavaModelException e) {
					CommonUIPlugin.getDefault().logError(e);
					return NO_CHILDREN;
				}
			}
			if (element instanceof IPackageFragmentRoot) {
				IPackageFragmentRoot pkgRoot = (IPackageFragmentRoot)element;
				try {
					return pkgRoot.getChildren();
				} catch (JavaModelException e) {
					return NO_CHILDREN;
				}
			}

			return super.getChildren(element);
		}

		protected Object[] getPackageFragmentRoots(IJavaProject project) throws JavaModelException {
			if (!project.getProject().isOpen()) {
				return NO_CHILDREN;
			}
			IPackageFragmentRoot[] roots= project.getPackageFragmentRoots();
			List<IPackageFragmentRoot> list= new ArrayList<IPackageFragmentRoot>(roots.length);
			for (int i= 0; i < roots.length; i++) {
				IPackageFragmentRoot root = roots[i];
				if (!isProjectPackageFragmentRoot(root) && root.getKind() == IPackageFragmentRoot.K_SOURCE) {
					list.add(root);
				}
			}
			return list.toArray();
		}
	}

	/**
	 * 
	 */
	public IFieldEditor createBrowseFileEditor(String name, String label, String defaultValue) {
		CompositeEditor editor = new CompositeEditor(name, label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new TextFieldEditor(name,label, defaultValue),
				new ButtonFieldEditor(name,createSelectFileAction(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_BROWS),defaultValue)});
		return editor;
	}

	/**
	 * 
	 */
	public IFieldEditor createButtonFieldEditor(String name, String label, String defaultValue, ButtonFieldEditor.ButtonPressedAction action, IValidator validator ) {
		CompositeEditor editor = new CompositeEditor(name,label, defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{new LabelFieldEditor(name,label),
				new TextFieldEditor(name,label, defaultValue),
				new ButtonFieldEditor(name,action,defaultValue)});
		return editor;
	}

	public IFieldEditor createButtonFieldEditor(String name, String label, String defaultValue, ButtonFieldEditor.ButtonPressedAction[] actions, IValidator validator ) {
		CompositeEditor editor = new CompositeEditor(name,label, defaultValue);
		List<IFieldEditor> editors = new ArrayList<IFieldEditor>();
		editors.add(new LabelFieldEditor(name,label));
		editors.add(new TextFieldEditor(name,label, defaultValue));
		for (int i = 0; i < actions.length; i++) {
			editors.add(new ButtonFieldEditor(name,actions[i],defaultValue));
		}
		editor.addFieldEditors(editors.toArray(new IFieldEditor[0]));
		return editor;
	}

	public IFieldEditor createButtonAndLinkFieldEditor(String name, String label, String defaultValue, ButtonFieldEditor.ButtonPressedAction buttonAction, ButtonFieldEditor.ButtonPressedAction linkAction, IValidator validator) {
		CompositeEditor editor = new CompositeEditor(name,label, defaultValue);
		List<IFieldEditor> editors = new ArrayList<IFieldEditor>();
		editors.add(new LabelFieldEditor(name,label));
		editors.add(new TextFieldEditor(name,label, defaultValue));
		editors.add(new ButtonFieldEditor(name, buttonAction, defaultValue));
		editors.add(new LinkFieldEditor(name, linkAction, defaultValue));
		editor.addFieldEditors(editors.toArray(new IFieldEditor[0]));
		return editor;
	}

	/**
	 * @param buttonName
	 * @return
	 */
	public ButtonFieldEditor.ButtonPressedAction createSelectFolderAction(String buttonName) {
		return new ButtonFieldEditor.ButtonPressedAction(buttonName) {
			@Override
			public void run() {
				DirectoryDialog dialog = new DirectoryDialog(Display.getCurrent().getActiveShell());
				dialog.setFilterPath(getFieldEditor().getValueAsString());
				dialog.setMessage(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_SELECT_SEAM_HOME_FOLDER);
				dialog.setFilterPath(getFieldEditor().getValueAsString());
				String directory = dialog.open();
				if(directory!=null) {
					getFieldEditor().setValue(directory);
				}
			}
		};
	}

	/**
	 * @param buttonName
	 * @return
	 */
	public ButtonFieldEditor.ButtonPressedAction createSelectWorkspaceFolderAction(String buttonName, final String initPath) {
		ButtonFieldEditor.ButtonPressedAction action = new ButtonFieldEditor.ButtonPressedAction(buttonName) {

			private String inerInitPath;

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.action.Action#run()
			 */
			@Override
			public void run() {
				ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(
						Display.getCurrent().getActiveShell(),
						new WorkbenchLabelProvider(), new WorkbenchContentProvider());
				dialog.setInput(ResourcesPlugin.getWorkspace());
				String path = inerInitPath!=null?inerInitPath:initPath;
				IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
				if (resource!=null) {
					dialog.setInitialSelection(resource);
				}
				dialog.addFilter(new ViewerFilter() {
					public boolean select(Viewer viewer, Object parentElement, Object element) {
						if (element instanceof IFolder || element instanceof IProject) {
							return true;
						}
						return false;
					}
				});
				dialog.setAllowMultiple(false);
				dialog.setTitle(CommonUIMessages.SELECT_WORKSPACE_FOLDER_DIALOG_TITLE); 
				dialog.setMessage(CommonUIMessages.SELECT_WORKSPACE_FOLDER_DIALOG_MESSAGE); 
				if (dialog.open() == Window.OK) {
					IResource res = (IResource) dialog.getFirstResult();
					IPath newPath = res.getFullPath();
					String value = newPath.toString();
					inerInitPath = value;
					getFieldEditor().setValue(value);
				}
			}
		};
		return action;
	}

	private static class SelectJavaPackageAction extends ButtonFieldEditor.ButtonPressedAction {
		private String defaultSourceFolderPath;

		public SelectJavaPackageAction(String buttonName, String defaultSourceFolderPath, String defaultPackageName) {
			super(buttonName);
			this.defaultSourceFolderPath = defaultSourceFolderPath;
		}

		@Override
		public void run() {
			String sourceFolder = (String)getFieldEditor().getData(IParameter.SOURCE_FOLDER_PATH);
			if(sourceFolder==null) {
				sourceFolder = defaultSourceFolderPath;
			}
			if(sourceFolder == null) {
				CommonUIPlugin.getDefault().logError("Can't init source folder");
				return;
			}
			IResource initSourceFolder = ResourcesPlugin.getWorkspace().getRoot().findMember(sourceFolder);
			if(initSourceFolder == null) {
				CommonUIPlugin.getDefault().logError("Can't find source folder: " + defaultSourceFolderPath);
				return;
			}
			IProject project = initSourceFolder.getProject();
			if(project == null) {
				CommonUIPlugin.getDefault().logError("Can't find project for: " + defaultSourceFolderPath);
				return;
			}
			IJavaProject javaProject = EclipseUtil.getJavaProject(project);
			if(javaProject == null) {
				CommonUIPlugin.getDefault().logError("Can't find java project for: " + defaultSourceFolderPath);
				return;
			}
			IPackageFragmentRoot packageFragmentRoot = null;
			IPackageFragmentRoot[] roots;
			try {
				roots = javaProject.getPackageFragmentRoots();
				for (int i = 0; i < roots.length; i++) {
					if (roots[i].getKind() == IPackageFragmentRoot.K_SOURCE && roots[i].getResource().equals(initSourceFolder)) {
						packageFragmentRoot = roots[i];
						break;
					}
				}
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
			if (packageFragmentRoot == null) {
				packageFragmentRoot = javaProject.getPackageFragmentRoot(javaProject.getResource());
			}
			if (packageFragmentRoot == null) {
				CommonUIPlugin.getDefault().logError("Can't find source folder for project " + project.getName());
				return;
			}
			IJavaElement[] packages = null;
			try {
				packages = packageFragmentRoot.getChildren();
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
			if (packages == null) {
				packages = new IJavaElement[0];
			}

			String initialValue = getFieldEditor().getValue().toString();
			IJavaElement initialElement = null;
			ArrayList<IJavaElement> packagesWithoutDefaultPackage = new ArrayList<IJavaElement>();
			for (IJavaElement packageElement : packages) {
				String packageName = packageElement.getElementName();
				if(packageName.length()>0) {
					packagesWithoutDefaultPackage.add(packageElement);
					if(packageName.equals(initialValue)) {
						initialElement = packageElement;
					}
				}
			}

			packages = (IJavaElement[])packagesWithoutDefaultPackage.toArray(new IJavaElement[packagesWithoutDefaultPackage.size()]);
			ElementListSelectionDialog dialog = new ElementListSelectionDialog(Display.getCurrent().getActiveShell(), new JavaElementLabelProvider(
					JavaElementLabelProvider.SHOW_DEFAULT));
			dialog.setTitle(J2EEUIMessages.PACKAGE_SELECTION_DIALOG_TITLE);
			dialog.setMessage(J2EEUIMessages.PACKAGE_SELECTION_DIALOG_DESC);
			dialog.setEmptyListMessage(J2EEUIMessages.PACKAGE_SELECTION_DIALOG_MSG_NONE);
			dialog.setElements(packages);
			if(initialElement!=null) {
				dialog.setInitialSelections(new Object[]{initialElement});
			}
			if (dialog.open() == Window.OK) {
				IPackageFragment fragment = (IPackageFragment) dialog.getFirstResult();
				if (fragment != null) {
					getFieldEditor().setValue(fragment.getElementName());
				} else {
					getFieldEditor().setValue("");
				}
			}
		}
	}

	/**
	 * 
	 * @param buttonName
	 * @return
	 */
	public ButtonFieldEditor.ButtonPressedAction createSelectFileAction(String buttonName) {
		return new ButtonFieldEditor.ButtonPressedAction(buttonName) {
			@Override
			public void run() {
				FileDialog dialog = new FileDialog(Display.getCurrent().getActiveShell());
				dialog.setFilterPath(getFieldEditor().getValueAsString());
				dialog.setText(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_SELECT_SEAM_HOME_FOLDER);
				dialog.setFilterPath(getFieldEditor().getValueAsString());
				String directory = dialog.open();
				if(directory!=null) {
					getFieldEditor().setValue(directory);
				}
			}
		};
	}

	/**
	 * 
	 */
	public ButtonFieldEditor.ButtonPressedAction createNotImplementedYetAction(String buttonName) {
		return new ButtonFieldEditor.ButtonPressedAction(buttonName) {
			@Override
			public void run() {
				new MessageDialog(Display.getCurrent().getActiveShell(), CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_ERROR, 
					null, CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_NOT_IMPLEMENTED_YET, MessageDialog.ERROR, new String[]{CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_OK},1)
				.open();
			}
		};
	}

	/**
	 * 
	 */
	public IFieldEditor createComboWithTwoButtons(String name, String label,
			List values, Object defaultValue, boolean flat,
			ButtonPressedAction action1, ButtonPressedAction action2,
			IValidator validator) {
		CompositeEditor editor = new CompositeEditor(name,label,defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{
				new LabelFieldEditor(name,label),
				new ComboFieldEditor(name,label, values, defaultValue, false),
				new ButtonFieldEditor(name, action1, defaultValue),
				new ButtonFieldEditor(name, action2, defaultValue)
		});
		return editor;
	}

	/**
	 * 
	 */
	public IFieldEditor createComboWithButton(String name, String label,
			List values, Object defaultValue, boolean flat,
			ButtonPressedAction action1,
			IValidator validator) {
		CompositeEditor editor = new CompositeEditor(name,label,defaultValue);
		editor.addFieldEditors(new IFieldEditor[]{
				new LabelFieldEditor(name,label),
				new ComboFieldEditor(name,label, values, defaultValue, false),
				new ButtonFieldEditor(name, action1, defaultValue)
		});
		return editor;
	}
}