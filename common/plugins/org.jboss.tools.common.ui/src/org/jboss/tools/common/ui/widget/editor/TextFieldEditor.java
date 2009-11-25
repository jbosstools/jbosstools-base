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


import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.widget.field.TextField;

/**
 * 
 * @author eskimo(dgolovin@exadel.com)
 *
 */
public class TextFieldEditor extends BaseFieldEditor implements PropertyChangeListener{
	
	/**
	 * 
	 */
	public static final int UNLIMITED = -1;
	
	protected int style = -1;
	
	/**
	 * 
	 * @param name
	 * @param aLabelText
	 * @param defaultvalue
	 */
	public TextFieldEditor(String name,String aLabelText,String defaultvalue) {
		super(name, aLabelText, defaultvalue);
	}
	
	/**
	 * 
	 * @param name
	 * @param aLabelText
	 * @param defaultvalue
	 * @param editable
	 */
	public TextFieldEditor(String name,String aLabelText,String defaultvalue,boolean editable) {
		super(name, aLabelText, defaultvalue);
		setEditable(editable);
	}	
	
	/**
	 * 
	 */
	protected TextField  fTextField = null;
	
	/**
	 * 
	 */
	protected int fWidthInChars = 0;

	/**
	 * 
	 */
	@Override
	public Object[] getEditorControls() {
		return new Control[] {getTextControl()};
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.BaseFieldEditor#doFillIntoGrid(java.lang.Object)
	 */
	@Override
	public void doFillIntoGrid(Object aParent) {
		Assert.isTrue(aParent instanceof Composite, CommonUIMessages.TEXT_FIELD_EDITOR_PARENT_CONTROL_SHOULD_BE_COMPOSITE);
		Assert.isTrue(((Composite)aParent).getLayout() instanceof GridLayout,CommonUIMessages.TEXT_FIELD_EDITOR_EDITOR_SUPPORTS_ONLY_GRID_LAYOUT);
		Composite aComposite = (Composite) aParent;
		Control[] controls = (Control[])getEditorControls(aComposite);
		GridLayout gl = (GridLayout)((Composite)aParent).getLayout();
		getTextControl(aComposite);

        GridData gd = new GridData();
        
        gd.horizontalSpan = gl.numColumns - 1;
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        
        fTextField.getTextControl().setLayoutData(gd);
	}

    /**
     * 
     * @param parent
     * @return
     */
    public Text getTextControl(Composite parent) {
        if (fTextField == null) {
        	fTextField = new TextField(parent, getInitialStyle());
            Text textField = fTextField.getTextControl();
            textField.setFont(parent.getFont());
            Object value = getValue();
            textField.setText(getValue().toString());
            textField.setEditable(isEditable());
            textField.setEnabled(isEnabled());
            fTextField.addPropertyChangeListener(this);
        } else if (parent!=null){
        	Assert.isTrue(parent==fTextField.getTextControl().getParent());
        }  
        return fTextField.getTextControl();
    }
    
    /**
     * 
     */
	protected void updateWidgetValues() {
		setValueAsString(getValueAsString());
	}

	/**
	 * 
	 * @return
	 */
	protected int getInitialStyle() {
		if(this.style >= 0) return style;
    	return SWT.SINGLE | SWT.BORDER;
    }

    /*
     * @param value
     * @return 
     */
    private String checkCollection(Object value){
    	
    	return value != null && (!((Collection)value).isEmpty()) ? prepareCollectionToString((Collection)value) : ""; //$NON-NLS-1$
    }
    
    /*
     * @param collection
     * @return 
     */
    private String prepareCollectionToString(Collection collection)
    {
    	String stringValue = ""; //$NON-NLS-1$
    	Object[] objects = collection.toArray();
    	for(int i = 0; i < objects.length; i++){
    		stringValue += objects[i];
    		if(i < objects.length - 1)
    			stringValue += " "; //$NON-NLS-1$
    	}
    	return stringValue;
    }
    
    
    /*
     * @param value
     * @return 
     */
    private String checkSimple(Object value){
    	return (value != null) ? value.toString() : ""; //$NON-NLS-1$
    }
    
    /**
     * 
     */
	@Override
	public int getNumberOfControls() {
		return 2;
	}

	/**
     * Returns this field editor's text control.
     *
     * @return the text control, or <code>null</code> if no
     * text field is created yet
     */
    protected Text getTextControl() {
        return fTextField!=null?fTextField.getTextControl():null;
    }

    /*
     * (non-Javadoc)
     * @see org.jboss.tools.seam.ui.widget.editor.BaseFieldEditor#setFocus()
     */
    @Override
	public boolean setFocus() {
    	boolean setfocus = false;
        if(fTextField!=null && !fTextField.getTextControl().isDisposed())
        	setfocus = fTextField.getTextControl().setFocus();
        return setfocus;
    }

    /**
     * 
     */
	@Override
	public Object[] getEditorControls(Object composite) {
		return new Control[]{getTextControl((Composite)composite)};
	}

	/**
	 * 
	 * @param object
	 */
	public void save(Object object) {
	}

	/**
	 * 
	 */
	@Override
	public void setValue(Object newValue) {
		super.setValue(newValue);
		if(fTextField!=null){
			fTextField.removePropertyChangeListener(this);
			fTextField.getTextControl().setText(newValue.toString());
			fTextField.addPropertyChangeListener(this);
		}
	}
	
	/**
	 * 
	 */
	@Override
	public void setEditable(boolean aEditable) {
		super.setEditable(aEditable);
		if(getTextControl()!=null) getTextControl().setEditable(aEditable);
	}
	
	/**
	 * 
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		super.setValue(evt.getNewValue());
	}
}