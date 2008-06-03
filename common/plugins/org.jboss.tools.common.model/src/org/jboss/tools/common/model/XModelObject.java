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
package org.jboss.tools.common.model;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.constraint.XProperty;
import org.jboss.tools.common.model.filesystems.XFileObject;

/**
 * Common interface for all objects in XModel.
 * At present is intended for immediate implementation 
 * by only XModelObjectImpl, other implementations should 
 * subclass it.
 * 
 * @author glory
 */
public interface XModelObject extends XProperty, XFileObject, IAdaptable {
	
	/**
	 * Returns model that created this model object.
	 * @return
	 */
	public XModel getModel();

	/**
	 * Returns Meta Data entity that defines the structure and 
	 * other functionality of this model object.
	 * @return entity object
	 */
	public XModelEntity getModelEntity();
	
	/**
	 * Returns parent model object.
	 * 
	 * @return parent model object or null.
	 */
    public XModelObject getParent();
    
    /**
     * Returns true if either this object is a root or it has 
     * root as its ancestor, and false otherwise.
     * 
     * @return
     */
    public boolean isActive();
    
    /**
     * Returns value of named attribute.
     * 
     * @param name
     * @return
     */
    public String getAttributeValue(String name);
    
    /**
     * Changes value of named attribute, if accepted by 
     * constraints, and notifies model listeners.
     * 
     * @param name
     * @param value
     * @return actual value set to attribute.
     */
    public String setAttributeValue(String name, String value);
    
    /**
     * Returns true if user input may be used to modify the object.
     * It is so usually when file is writable.
     * @return
     */
    public boolean isObjectEditable();
    
    /**
     * Returns true if user input may be used to modify the attribute.
     * Sometimes, if it is advisable to change the attribute with 
     * specific wizard and to restrict user access to the attribute 
     * in the property view, it is preferable to return false.
     * 
     * @param name
     * @return
     */
    public boolean isAttributeEditable(String name);
    
    /**
     * Returns true if object has at least one child object and 
     * false otherwise.
     * 
     * @return boolean
     */
    public boolean hasChildren();
    
    /**
     * Returns array of children objects.
     * 
     * @return array of model objects
     */
    public XModelObject[] getChildren();
    
    /**
     * Returns child model object that has index i in the array 
     * returned by getChildren() method.
     * 
     * @param i
     * @return
     */
    public XModelObject getChildAt(int i);
    
    /**
     * Returns array of children with specified entity name.
     * @param entityName
     * 
     * @return array of model objects
     */
    public XModelObject[] getChildren(String entityName);
    
    /**
     * Returns child object by path that contains path parts 
     * separated by "/".
     * 
     * @param path
     * @return child model object or null
     */
    public XModelObject getChildByPath(String path);
    
    /**
     * Returns array of children model objects  
     * to be processed by the loader. 
     * 
     * @return array of model objects
     */
    public XModelObject[] getChildrenForSave();
    
    /**
     * Returns a path part that makes this object unique 
     * in the set of children of its parent. This model object 
     * can be obtained from its parent by method getChildByPath()
     * with value returned by this method.
     * 
     * @return path part
     */
    public String getPathPart();
    
    /**
     * Returns path that equals to
     * - empty string for the model root;
     * - path part for an immediate child of the model root;
     * - "root:" plus path part for an extra model root;
     * - parent path plus separator "/" plus path part for any
     * other model object.

     * @return
     */
    public String getPath();
    
    /**
     * Adds child model object to this model object. 
     * Returns true if method successes and false if it fails. 
     * Method fails if
     * - entity of child object is not a child entity of this object;
     * - this object has a child with the same path part as that of 
     * the parameter.
     *  
     * @param object
     * @return boolean
     */
    public boolean addChild(XModelObject object);
    
    /**
     * Removes model object from children set of this object,
     * if it was its child, and does nothing otherwise. 
     * If object is removed from parent children, its method 
     * getParent() returns null.
     * 
     * @param child
     */
    public void removeChild(XModelObject child);
    
    /**
     * Convenience method that checks if the object 
     * has a parent, and removes this object from its children.
     */
    public void removeFromParent();
    
    /**
     * Creates copy of this object. Same as copy(true).
     * @return
     */
    public XModelObject copy();
    
    /**
     * Creates copy of this object. If transform=true, 
     * Entity of copied object may differ from entity 
     * of this object.
     * 
     * @param transform
     * @return
     */
    public XModelObject copy(boolean transform);

    /**
     * Creates copy of this object. Same as copy(false, level).
     * @return
     */
    public XModelObject copy(int level);
    
    /**
     * Creates copy of this object. If transform=true, 
     * Entity of copied object may differ from entity 
     * of this object. If level = 0, children are not copied,
     * if level > 0 children are copied as copy(transform, level - 1).

     * @param transform
     * @param level
     * @return
     */
    public XModelObject copy(boolean transform, int level);
    
    /**
     * Returns flag modified.
     * If true object is supposed to be changed.
     * 
     * @return
     */
    public boolean isModified();
    
    /**
     * Flips flag modified.
     * @param b
     */
    public void setModified(boolean b);
    
    /**
     * Returns string to be presented by tools to user.
     * 
     * @return
     */
    public String getPresentationString();
    
    /**
     * Returns id to be used for constracting image.
     * @return
     */
    public String getMainIconName();
    
    /**
     * Returns image to be presented by tools to user.
     * @return
     */
	public Image getImage();

    
    /**
     * 
     * @param types
     * @return
     */
	public Image getImage(String[] types);
    
    /**
     * Returns counter of changes to be used by classes, 
     * that cache data from this object, to decide if 
     * cached data is up to date.
     *  
     * @return
     */
    public long getTimeStamp();
	public long getLastModificationTimeStamp();
	
	/**
	 * Compares this object to another object. Returns true if all 
	 * of the following is true:
	 * - entity names are equal;
	 * - attributes with property copyable=true are equal;
	 * - arrays returned by method getChildrenForSave()
	 * have equal lengths and for each index value corresponding
	 * model objects are equal with respect to this method.
	 * 
	 * @param o
	 * @return
	 */
    public boolean isEqual(XModelObject o);
    
    /**
     * Convenience method that notifies model listeners that 
     * this object is changed.
     * 
     * @param details
     */
	public void fireObjectChanged(Object details);
	
	/**
	 * Returns IMarker.SEVERITY_INFO or IMarker.SEVERITY_WARNING or
	 * IMarker.SEVERITY_ERROR
	 *
	 * @return
	 */
	public int getErrorState();
	
	/**
	 * Returns true there is an error marker registered to 
	 * the relevant resource and associated with this attribute.
	 * 
	 * @param attributeName
	 * @return
	 */
	public boolean getAttributeErrorState(String attributeName);
	
	/**
	 * Notifies this object that it or its has 
	 * errors or only warnings or none of them.
	 * This method is intended to be called only by 
	 * XMarkerManager and by implementations of 
	 * auxiliary model objects that need to compute 
	 * their error state by checking error states 
	 * of objects that are not their children.
	 * 
	 * @param b
	 */
	public void setErrorState(int b);
	
	/**
	 * Returns number of children that have error markers
	 * associated with them.
	 * 
	 * @return
	 */
	public int getErrorChildCount();

	/**
	 * Returns number of children that have only warning 
	 * markers associated with them.
	 * 
	 * @return
	 */
	public int getWarningChildCount();

}
