/*************************************************************************************
 * Copyright (c) 2009-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.ui.buildpath.dialog;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.jboss.tools.common.jdt.ui.JDTExtUIActivator;

public class MaterializeLibraryDialog extends TitleAreaDialog {

  private static final String TITLE = "Materialize ";

  private static final String SOURCE_PROPERTY = "SOURCE_PROPERTY";

  private static final String FILENAME_PROPERTY = "FILENAME_PROPERTY";

  private IFolder libFolder;
  private Map<IClasspathEntry, String> classpathEntryPaths;
  private Map<IPath, String> selectedClasspathEntryPaths;

  private final String libName;

  private Image jarImage;
  private Image projectImage;

  // private StringButtonDialogField libFolderDialogField;

  private Text libfolderText;

  private CheckboxTableViewer classpathEntriesViewer;

  public MaterializeLibraryDialog(Shell shell, IProject project, IClasspathContainer containerToMaterialize, String defaultLib) {
    super(shell);
    setShellStyle(super.getShellStyle() | SWT.RESIZE | SWT.MODELESS);
    this.libName = containerToMaterialize.getDescription();
    IPath folderPath = project.getFullPath().append(defaultLib);
    libFolder = ResourcesPlugin.getWorkspace().getRoot().getFolder(folderPath);
    initClasspathEntryPaths(containerToMaterialize);
  }

  private void initClasspathEntryPaths(IClasspathContainer container) {
    IClasspathEntry[] cpEntries =  container.getClasspathEntries();
    classpathEntryPaths = new LinkedHashMap<IClasspathEntry, String>(cpEntries.length);
    for (IClasspathEntry entry : cpEntries) {
      if ((entry.getEntryKind() == IClasspathEntry.CPE_LIBRARY && entry.getPath() != null)
          || (entry.getEntryKind() == IClasspathEntry.CPE_PROJECT)) {
          IPath sourceFilePath = entry.getPath();
          String fileName = sourceFilePath.lastSegment();
          classpathEntryPaths.put(entry, fileName);
      }
    }
  }

  @Override
  protected void configureShell(Shell shell) {
    super.configureShell(shell);
    shell.setText(TITLE + "Classpath Library");
  }

  private void initImages() {
	  jarImage =JDTExtUIActivator.getJarIcon();
	  projectImage= JDTExtUIActivator.getProjectIcon();
  }
  
  
  @Override
  protected Control createDialogArea(Composite parent) {
	  
	initImages();
	setTitle(TITLE + libName);
	   	  
    Composite area = (Composite) super.createDialogArea(parent);

    Composite container = new Composite(area, SWT.NONE);
    container.setEnabled(true);

    GridLayout layout = new GridLayout(3, false);
    layout.marginLeft = 12;
    container.setLayout(layout);
    container.setLayoutData(new GridData(GridData.FILL_BOTH));

    setMessage("Copy selected jars from " + libName + " to the destination folder.");

    // ContainerFieldAdapter adapter= new ContainerFieldAdapter();
    // libFolderDialogField = new StringButtonDialogField(adapter);
    // libFolderDialogField.setLabelText("Destination folder");
    // libFolderDialogField.setButtonLabel("Browse...");
    // libFolderDialogField.doFillIntoGrid(container,
    // libFolderDialogField.getNumberOfControls());
    // LayoutUtil.setWidthHint(libFolderDialogField.getTextControl(null),
    // convertWidthInCharsToPixels(40));

    Label libFolderLabel = new Label(container, SWT.NONE);
    libFolderLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
    libFolderLabel.setText("Destination folder");

    libfolderText = new Text(container, SWT.BORDER);
    libfolderText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
    libfolderText.setEditable(true);
    libfolderText.setText(libFolder.getFullPath().toPortableString());
    libfolderText.addModifyListener(new ModifyListener() {
		@Override
		public void modifyText(ModifyEvent event) {
			validate();
		}
	});

    addSelectFolderButton(container);
    // libFolderDialogField.setText(libFolder.getFullPath().toPortableString());
    displayClasspathEntriesTable(container);

    return area;
  }

  private void addSelectFolderButton(Composite container) {
	  
	  Button button = new Button(container, SWT.NONE);
	  button.setLayoutData(new GridData(SWT.FILL, SWT.UP,
				false, false, 1, 1));
	  button.setText("Select Folder");
	  button.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				  ContainerSelectionDialog dialog = new ContainerSelectionDialog(getShell(), getLibFolderFromText(libfolderText.getText()), true, "Select Destination Folder");
				  dialog.setTitle("Container Selection");
				  dialog.open();
				  Object[] result = dialog.getResult();
				  if (result != null && result[0] instanceof IPath) {
					  libfolderText.setText(((IPath)result[0]).toPortableString());
				  }
			}
			
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
  }

  @Override
  public boolean close() {
    if (jarImage != null) {
      jarImage.dispose();
	}
    if (projectImage!= null) {
    	projectImage.dispose();
  	}
    return super.close();
  }
  
  private void displayClasspathEntriesTable(Composite container) {
    GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 4);
    gd.heightHint = 500;
    gd.widthHint = 600;

    classpathEntriesViewer = CheckboxTableViewer.newCheckList(container, SWT.BORDER | SWT.MULTI);
    Table table = classpathEntriesViewer.getTable();
    table.setFocus();
    table.setLayoutData(gd);
    table.setLinesVisible(true);
    table.setHeaderVisible(true);

    TableColumn emptyColumn = new TableColumn(table, SWT.NONE);
    emptyColumn.setWidth(20);

    TableViewerColumn sourceColumn = new TableViewerColumn(classpathEntriesViewer, SWT.NONE);
    sourceColumn.getColumn().setText("Source ");
    sourceColumn.getColumn().setWidth(300);


    TableViewerColumn destinationColumn = new TableViewerColumn(classpathEntriesViewer, SWT.NONE);
    destinationColumn.getColumn().setText("Copy as...");
    destinationColumn.getColumn().setWidth(200);

    classpathEntriesViewer.setContentProvider(ArrayContentProvider.getInstance());
    classpathEntriesViewer.setLabelProvider(new ClasspathEntryLabelProvider());
    classpathEntriesViewer.addCheckStateListener(new ICheckStateListener() {
      public void checkStateChanged(CheckStateChangedEvent event) {
        refresh();
      }
    });
    classpathEntriesViewer.setInput(classpathEntryPaths.entrySet());
    classpathEntriesViewer.setAllChecked(true);

    
    addSelectionButton(container, "Select All", true);
	addSelectionButton(container, "Deselect All", false);
	
    addTableListeners();

  }

  private void addTableListeners() {
    addCellEditors();
  }

  protected void addCellEditors() {
    classpathEntriesViewer.setColumnProperties(new String[] {
        "EMPTY", SOURCE_PROPERTY, FILENAME_PROPERTY });

    TextCellEditor ce = new TextCellEditor(classpathEntriesViewer.getTable()); 
//    ce.setValidator(new ICellEditorValidator() {
//		@Override
//		public String isValid(Object arg0) {
//			String name = arg0.toString();
//			return (checkValidName(name))?null:name;
//		}
//	});
	
    CellEditor[] editors = new CellEditor[] {
        null,
        new TextCellEditor(classpathEntriesViewer.getTable()),
        ce };
    	
    classpathEntriesViewer.setCellEditors(editors);
    classpathEntriesViewer.setCellModifier(new FileNameCellModifier());
  }

  public Map<IPath, String> getSelectedClasspathEntryPaths() {
    return selectedClasspathEntryPaths;
  }

  public IFolder getLibFolder() {
    return libFolder;
  }

  private static IFolder getLibFolderFromText(String text) {
    String portablePath = text.replaceAll("\\\\", "/");
    IPath path = new Path(portablePath);
    return ResourcesPlugin.getWorkspace().getRoot().getFolder(path);
  }

  @Override
  protected void okPressed() {
	if (!validate()) {
		return;
	}
    libFolder = getLibFolderFromText(libfolderText.getText());
    super.okPressed();
  }

  private boolean validate() {
	  boolean valid = validateLibFolder() && validateEntries();
	  if (valid) {
		  setErrorMessage(null);
	  }
	  return valid;
  }
		
  private boolean validateLibFolder() {
	IFolder folder = getLibFolderFromText(libfolderText.getText());
	String ancestorPath = folder.getFullPath().segment(0);
    IResource ancestor = ResourcesPlugin.getWorkspace().getRoot().findMember(ancestorPath);
    if (ancestor == null || !ancestor.exists()) {
    	setErrorMessage(ancestorPath + " does not exist ");
    	return false;
    }
    return true;
  }

  private boolean validateEntries() {
    Object[] selection = classpathEntriesViewer.getCheckedElements();
    selectedClasspathEntryPaths = new LinkedHashMap<IPath, String>(selection.length);
    for (Object o : selection) {
      @SuppressWarnings("unchecked")
      Map.Entry<IClasspathEntry, String> entry = (Map.Entry<IClasspathEntry, String>)o;
      if (entry.getKey().getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
	      String name = entry.getValue();
	      if (!checkValidName(name)) {
	    	  setErrorMessage(name + " is not a valid file name");
	    	  return false;
	      }
      }
      selectedClasspathEntryPaths.put(entry.getKey().getPath(),entry.getValue());
    }
    
    Set<String> duplicates = findDuplicates(selectedClasspathEntryPaths.values()); 
    if (!duplicates.isEmpty()) {
  	  setErrorMessage("Duplicate entries found : "+duplicates.toString());
  	  return false;
    }
  	return true;
  }

  private Set<String> findDuplicates(Collection<String> values) {
	Set<String> uniqueNames = new HashSet<String>(values.size());
	Set<String> duplicateNames = new HashSet<String>();
	for (String name : values) {
		if (!uniqueNames.add(name)) {
			duplicateNames.add(name);
		}
	}
	return duplicateNames;
}

private class ClasspathEntryLabelProvider extends LabelProvider implements ITableLabelProvider {

    private static final int SOURCE_COLUMN = 1;
    private static final int FILENAME_COLUMN = 2;

    public String getColumnText(Object element, int columnIndex) {
      Map.Entry<IClasspathEntry, String> entry = (Map.Entry<IClasspathEntry, String>) element;
      StringBuilder text = new StringBuilder();
      if (entry != null) {
        if (columnIndex == SOURCE_COLUMN) {
          text.append(entry.getKey().getPath().lastSegment());
        } else if (columnIndex == FILENAME_COLUMN){
          text.append(entry.getValue());
        }
      }

       return text.toString();
    }

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
      Image img = null;
      if (columnIndex > 0) {
        Map.Entry<IClasspathEntry, String> entry = (Map.Entry<IClasspathEntry, String>) element;
        if (entry.getKey().getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
          img = jarImage;
        } else {
          img = projectImage;
        }
      }
      return img;
    }
  }

  private Button addSelectionButton(Composite container, String label, final boolean ischecked) {
		Button button = new Button(container, SWT.NONE);
		button.setLayoutData(new GridData(SWT.FILL, SWT.UP,
				false, false, 1, 1));
		button.setText(label);
		button.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				classpathEntriesViewer.setAllChecked(ischecked);
				refresh();
			}

			public void widgetDefaultSelected(SelectionEvent e) {

			}
		});
		
		return button;
	}
  
  protected void refresh() {
	  classpathEntriesViewer.refresh();
  }

  private class FileNameCellModifier implements ICellModifier {

    public boolean canModify(Object element, String property) {
      Map.Entry<IClasspathEntry, String> entry = (Map.Entry<IClasspathEntry, String>) element;
      return (entry.getKey().getEntryKind() == IClasspathEntry.CPE_LIBRARY) && 
    		  (FILENAME_PROPERTY.equals(property) || SOURCE_PROPERTY.equals(property)) ;
    }

    public Object getValue(Object element, String property) {
      Map.Entry<IClasspathEntry, String> entry = (Map.Entry<IClasspathEntry, String>) element;
      //if(entry.getKey().getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
        if (property.equals(SOURCE_PROPERTY)) {
          return entry.getKey().getPath().toOSString();
        } else if (property.equals(FILENAME_PROPERTY)) {
          return entry.getValue();
        }
      //}
      return ""; //$NON-NLS-1$
    }

    public void modify(Object element, String property, Object value) {
      if (property.equals(FILENAME_PROPERTY)) {
        TableItem item = (TableItem)element;
        Map.Entry<IClasspathEntry, String> entry = (Map.Entry<IClasspathEntry, String>) item.getData();
        if(entry.getKey().getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
          String name = value.toString();
          entry.setValue(name);
          validate();
          classpathEntriesViewer.refresh();
        }
      }
    }
  }

	public boolean checkValidName(String name) {
		//TODO checks for :
		// - duplicates
		// - existing CPE
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IStatus result = workspace.validateName(name, IResource.FILE);
		if (!result.isOK()) {
			 return false;
		}
		return  !name.contains("\\") &&
				(name.endsWith(".jar") || name.endsWith(".zip"));
	}

}
