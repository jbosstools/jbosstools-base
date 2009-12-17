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

import java.io.*;
import java.util.*;
import org.jboss.tools.common.meta.XModelMetaData;
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.model.undo.XUndoManager;
import org.jboss.tools.common.model.loaders.EntityRecognizer;
import org.jboss.tools.common.model.loaders.EntityRecognizerExtension;
import org.jboss.tools.common.model.filesystems.impl.FileSystemPeer;

/**
 * Interface for general object model. The model reduces 
 * to common functionality any structure of objects 
 * of different nature. For instance, it is used
 * for loading to a united structure a disk file system and 
 * subelements of any file (*.xml, *.properties, *.java, etc.).
 *
 * The interface is intended for implementation by only one 
 * class. Models with different behaviour should be obtained 
 * with using different definitions from the Meta Data.
 * 
 * @author glory
 */
public interface XModel {

	/**
	 * Returns Meta Data - definitions of object structure, 
	 * attributes and actions. Meta Data is singleton, it is
	 * shared by all models.
	 *  
	 * @return Meta Data
	 */
	public XModelMetaData getMetaData();
	
	/**
	 * Properties passed to model at creation. They include 
	 * all System properties as well as specific data used by 
	 * the model instance (i.e. model of IProject contains 
	 * entry "project" with value of IProject instance).
	 * 
	 * @return model properties
	 */
	public Properties getProperties();
	
	/**
	 * Adds listener to the model instance.
	 * All changes in model are fired only to listeners 
	 * implementing XModelTreeListener interface.
	 * 
	 * @param listener
	 */
	public void addModelTreeListener(XModelTreeListener listener);
	
	/**
	 * Removes listener from the model instance.
	 * @param listener
	 */
	public void removeModelTreeListener(XModelTreeListener listener);
	
	/**
	 * Returns root object. Root object is defined by String property
	 * "rootEntity", passed to model at creation. 
	 * By default rootEntity=Root.
	 *  
	 * @return
	 */
	public XModelObject getRoot();
	
	/**
	 * Returns a named root object with path registered 
	 * in meta mapping "Roots". 
	 * 
	 * @param name
	 * @return
	 */
	public XModelObject getRoot(String name);
	
	/**
	 * Creates new model object with specified entity name and 
	 * properties (usually attribute values). The created object is 
	 * not added to the structure. 
	 * If model fails to create object, it writes the problem 
	 * to the log and returns null.
	 * 
	 * @param entityName 
	 * @param properties Properties or null.
	 * @return Created model object.
	 */
	public XModelObject createModelObject(String entityName, Properties properties);
	
	/**
	 * Returns model object by String path. The path contains parts
	 * separated by XModelObjectConstants.SEPARATOR. The root has empty string path. 
	 * For convenience, to find object in any file system (which is 
	 * an immediate child of "FileSystems" sub-root), path may be 
	 * started with XModelObjectConstants.SEPARATOR. 
	 * To get so-called "extra-root" which defines an auxiliary 
	 * structure not connected to the structure of the main root object, 
	 * path may be started with "root:".
	 * Otherwise, path is started by the path part of an immediate 
	 * child of the main root object. 
	 * 
	 * @param path
	 * @return
	 */
	public XModelObject getByPath(String path);
	
	/**
	 * Changes attribute value of specified model object if 
	 * constraints accept the new value, sets modified=true 
	 * and notifies model listeners. 
	 *  
	 * @param object
	 * @param attributeName
	 * @param newValue
	 */
	public void changeObjectAttribute(XModelObject object, String attributeName, String newValue) throws XModelException;
	
	/**
	 * Does the same as method changeObjectAttribute, but also 
	 * shows error dialog if constraints do not accept the new value, 
	 * and otherwise calls method onAttributeValueEdit on the modified 
	 * model object to make dependent model changes implementd in it.
	 * This method is intended to be called by tools processing 
	 * user input.
	 * 
	 * @param object
	 * @param attributeName
	 * @param newValue
	 */
	public void editObjectAttribute(XModelObject object, String attributeName, String newValue) throws XModelException;
	
	/**
	 * Returns undo manager that keeps multi-changes transaction 
	 * that can be rolled back if failed.
	 * 
	 * @return
	 */
	public XUndoManager getUndoManager();
	
	/**
	 * Returns convenience manager that allows to bind listeners
	 * that listen only to changes to some specified objects rather 
	 * than all model changes.
	 * 
	 * @return
	 */
	public XModelChangeManager getChangeManager();
	
	/**
	 * Sets singleton ServiceDialog implementation shared by all models.
	 * @param service
	 */
	public void setService(ServiceDialog service);
	
	/**
	 * Returns singleton ServiceDialog implementation shared by all models.
	 * @return
	 */
	public ServiceDialog getService();
	
	/**
	 * Returns model buffer that keeps last copied model objects.
	 * @return
	 */
	public XModelBuffer getModelBuffer();
	
	/**
	 * Returns a singleton used to resolve entity of a file object.
	 * @return
	 */
	public EntityRecognizerExtension getEntityRecognizer();
	
	/**
	 * Returns registry keeping time stamps of loaded file objects.
	 * @return
	 */
	public FileSystemPeer getFileRegistry();
	
	/**
	 * Loads the model.
	 * Implementation of the method invokes load() method on
	 * object loader of root object. During loading all model 
	 * listeners are deactivated. After loading, model fires 
	 * event XModelTreeEvent.STRUCTURE_CHANGED with root object.
	 */
	public void load();

	/**
	 * Saves the model.
	 * Implementation of the method invokes update() method on
	 * object loader of root object.
	 */
	public void update() throws XModelException;

	/**
	 * Saves the model.
	 * Implementation of the method invokes save() method on
	 * object loader of root object.
	 */
	public void save();
	
	/**
	 * Convenience method that saves sub-root "%XStudio%".
	 * Presently, only preference model has that subroot
	 * so that for the preference model this method 
	 * is equal to save() and for other models it does notning. 
	 */
	public void saveOptions();
	
	/**
	 * obsolete
	 * @return
	 */
	public PrintWriter getOut();
	
	/**
	 * obsolete
	 * @param out
	 */
	public void setOut(PrintWriter out);
	
	/** 
	 * Registers in model an object by unique id.
	 * Manager is any object used to extend model 
	 * functionality not implemented by its traditional 
	 * framework (Mata Data + XModelObject)
	 * 
	 * @param id
	 * @param manager
	 */	
	public void addManager(String id, Object manager);

	/**
	 * Returns an object registered by id. 
	 * 
	 * @param id
	 * @return
	 */
	public Object getManager(String id);
	
	/**
	 * Removes an object registered by id from model.
	 * 
	 * @param id
	 */
	public void removeManager(String id);

}
