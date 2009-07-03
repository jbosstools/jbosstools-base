/*
 * Created on 24.02.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.jboss.tools.common.model.ui.forms;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.WhiteSettings;

/**
 * @author Eskimo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class FormData implements IFormData {

	private String header; // (translatable)
	private String description; // (translatable)
	private IFormAttributeData[] attributes;
	private String[] entities; //  (non-translatable)
	private IFormActionData[] actions;
	private IFormData[] forms;
	private String entityName; //  (non-translatable)
	private IWidgetSettings widgetSettings;

	private String formClassName;

	/**
	 * 
	 * @param header (translatable)
	 * @param description (translatable)
	 * @param entityName (non-translatable)
	 * @param attributes
	 * @param entities (non-translatable)
	 * @param actions
	 * @param forms
	 * @param widgetSettings
	 */
	public FormData(String header, String description, String entityName, IFormAttributeData[] attributes, String[] entities, IFormActionData[] actions, IFormData[] forms, IWidgetSettings widgetSettings) {
		this.header = header;
		this.description = description;
		this.entityName = entityName;
		this.attributes = attributes;
		this.entities = entities;
		this.actions = actions;
		this.forms = forms;
		this.widgetSettings = widgetSettings;
	}

	/**
	 * 
	 * @param header (translatable)
	 * @param description (translatable)
	 * @param entityName (non-translatable)
	 * @param attributes
	 * @param entities (non-translatable)
	 * @param actions
	 */
	public FormData(String header, String description, String entityName, IFormAttributeData[] attributes, String[] entities, IFormActionData[] actions) {
		this(header, description, entityName, attributes, entities, actions, null, new WhiteSettings());
	}

	/**
	 * 
	 * @param header (translatable)
	 * @param description (translatable)
	 * @param attributes
	 * @param entities (non-translatable)
	 * @param actions
	 */
	public FormData(String header, String description, IFormAttributeData[] attributes, String[] entities, IFormActionData[] actions) {
		this(header, description, null, attributes, entities, actions, null, new WhiteSettings());
	}

	/**
	 * 
	 * @param entityName (non-translatable)
	 * @param entities (non-translatable)
	 * @param forms
	 */
	public FormData(String entityName, String[] entities, IFormData[] forms) {
		this(null, null, entityName, null, entities, null, forms, new WhiteSettings());
	}

	/**
	 * 
	 * @param header (translatable)
	 * @param description (translatable)
	 * @param entityName (non-translatable)
	 * @param attributes
	 */
	public FormData(String header, String description, String entityName, IFormAttributeData[] attributes) {
		this(header, description, entityName, attributes, null, null, null, new WhiteSettings());
	}

	/**
	 * 
	 * @param header (translatable)
	 * @param description (translatable)
	 * @param attributes
	 */
	public FormData(String header, String description, IFormAttributeData[] attributes) {
		this(header, description, null, attributes, null, null, null, new WhiteSettings());
	}

	/**
	 * 
	 * @param entityName (non-translatable)
	 * @param forms
	 */
	public FormData(String entityName, IFormData[] forms) {
		this(null, null, entityName, null, null, null, forms, null);
	}

	/**
	 * 
	 * @param formClassName (non-translatable)
	 */
	public FormData(String formClassName) {
		this.formClassName = formClassName;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IFormData#getAttributes()
	 */
	public IFormAttributeData[] getAttributes() {
		return attributes;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IFormData#getEntities()
	 */
	public String[] getEntities() {
		return entities;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IFormData#getActions()
	 */
	public IFormActionData[] getActions() {
		return actions;
	}

    /* (non-Javadoc)
     * @see org.jboss.tools.common.model.ui.forms.IFormData#getForms()
     */
    public IFormData[] getForms() {
    	return forms;
    }

    /* (non-Javadoc)
     * @see org.jboss.tools.common.model.ui.forms.IFormData#getEntityName()
     */
    public String getEntityName() {
        return entityName;
    }

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IFormData#getWidgetSettings()
	 */
	public IWidgetSettings getWidgetSettings() {
		return widgetSettings;
	}

    /* (non-Javadoc)
     * @see org.jboss.tools.common.model.ui.forms.IFormData#getDescription()
     */
    public String getDescription() {
        return description;
    }

    /* (non-Javadoc)
     * @see org.jboss.tools.common.model.ui.forms.IFormData#getHeader()
     */
    public String getHeader() {
        return header;
    }

    /* (non-Javadoc)
     * @see org.jboss.tools.common.model.ui.forms.IFormData#getFormClassName()
     */
    public String getFormClassName() {
    	return formClassName;
    }

    /* (non-Javadoc)
     * @see org.jboss.tools.common.model.ui.forms.IFormData#isNotLayouredForm()
     */
    public boolean isNotLayouredForm() {
    	return formClassName!=null;
    }
}