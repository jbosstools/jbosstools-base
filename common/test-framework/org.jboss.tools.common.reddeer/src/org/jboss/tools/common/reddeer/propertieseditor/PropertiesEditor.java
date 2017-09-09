/*******************************************************************************
 * Copyright (c) 2015-2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.reddeer.propertieseditor;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.reddeer.core.lookup.EditorPartLookup;
import org.eclipse.reddeer.core.util.Display;
import org.eclipse.reddeer.core.util.ResultRunnable;
import org.eclipse.reddeer.swt.api.Table;
import org.eclipse.reddeer.swt.api.TableItem;
import org.eclipse.reddeer.swt.impl.button.FinishButton;
import org.eclipse.reddeer.swt.impl.button.PushButton;
import org.eclipse.reddeer.swt.impl.ctab.DefaultCTabItem;
import org.eclipse.reddeer.swt.impl.shell.DefaultShell;
import org.eclipse.reddeer.swt.impl.table.DefaultTable;
import org.eclipse.reddeer.swt.impl.text.DefaultText;
import org.eclipse.reddeer.workbench.impl.editor.DefaultEditor;
/**
 * RedDeer implementation of JBoss Properties file editor
 * @author vlado pakan
 *
 */
public class PropertiesEditor extends DefaultEditor {
	  
	public PropertiesEditor(String title) {
		super(title);
	}

	public void activateSourceTab() {
		activateEditorCTabItem("Source");
	}

	public void activatePropertiesTab() {
		activateEditorCTabItem("Properties");
	}

	private void activateEditorCTabItem(String tabItemLabel) {
		activate();
		new DefaultCTabItem(tabItemLabel).activate();
	}

	public PropertiesSourceEditor getPropertiesSourceEditor() {
		activateSourceTab();
		final org.jboss.tools.common.propertieseditor.PropertiesCompoundEditor pce = ((org.jboss.tools.common.propertieseditor.PropertiesCompoundEditor) (EditorPartLookup.getInstance().getActiveEditor()));
		ITextEditor iTextEditor = (ITextEditor) Display.syncExec(new ResultRunnable<IEditorPart>() {
			@Override
			public IEditorPart run() {
				return pce.getActiveEditor();
			}
		});
		return new PropertiesSourceEditor(iTextEditor);
	}
	
	public void addProperty(String propertyName, String propertyValue){
	    getAddButton().click();
	    new DefaultShell("Add Property");
		new DefaultText(0).setText(propertyName);
	    new DefaultText(1).setText(propertyValue);
	    new FinishButton().click();
	}

	public Table getPropertiesTable (){
		activatePropertiesTab();
		return new DefaultTable();
	}
	
	public TableItem getProperty (int row){
		return getPropertiesTable().getItem(row);
	}
	
	public TableItem getProperty (String value){
		return getPropertiesTable().getItem(value);
	}
	
	public String getPropertyName (int row){
		return getProperty(row).getText(1);
	}
	
	public String getPropertyValue (int row){
		return getProperty(row).getText(0);
	}
	
	public String getPropertyValue (String propertyName){
		return getProperty(propertyName).getText(1);
	}
	
	public void clickOnPropertyValue (String propertyName){
		getProperty(propertyName).click(1);
	}
	
	public void clickOnPropertyValue (int row){
		getProperty(row).click(1);
	}
	public void clickOnPropertyName (String propertyName){
		getProperty(propertyName).click(0);
	}
	
	public void clickOnPropertyName (int row){
		getProperty(row).click(0);
	}
	
	public int getPropertiesCount (){
		return getPropertiesTable().rowCount();
	}
	
	public void selectProperty (int row){
		getProperty(row).select();
	}
	
	public void selectProperty (String value){
		getProperty(value).select();
	}
	public PushButton getAddButton (){
		activatePropertiesTab();
		return new PushButton("Add");
	}
	public PushButton getEditButton (){
		activatePropertiesTab();
		return new PushButton("Edit");
	}
	public PushButton getDeleteButton (){
		activatePropertiesTab();
		return new PushButton("Delete");
	}
	public PushButton getUpButton (){
		activatePropertiesTab();
		return new PushButton("Up");
	}
	public PushButton getDownButton (){
		activatePropertiesTab();
		return new PushButton("Down");
	}
}