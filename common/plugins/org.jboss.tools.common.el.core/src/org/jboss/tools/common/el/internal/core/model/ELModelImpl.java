/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.internal.core.model;

import java.util.ArrayList;
import java.util.List;

import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.parser.SyntaxError;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELModelImpl extends ELObjectImpl implements ELModel {
	List<SyntaxError> errors = null;
	String source;
	List<ELInstance> instances = new ArrayList<ELInstance>();
	int delta = 0;

	public ELModelImpl() {}

	public ELModelImpl getModel() {
		return this;
	}

	public String getSource() {
		return source;
	}

	public List<ELInstance> getInstances() {
		return instances;
	}

	public void setSource(String source) {
		this.source = source;
	}

	public void addChild(ELObjectImpl child) {
		if(child instanceof ELInstanceImpl) {
			addInstance((ELInstanceImpl)child);
		} else {
			throw new IllegalArgumentException("EL root can have onle ELInstances as its children.");
		}
	}

	public void addInstance(ELInstanceImpl instance) {
		super.addChild(instance);
		instances.add(instance);
	}

	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (ELInstance p: instances) {
			sb.append(p.toString());
		}
		return sb.toString();
	}

	public ELObjectType getType() {
		return ELObjectType.EL_MODEL;
	}

	public void setErrors(List<SyntaxError> errors) {
		this.errors = errors;
		for (SyntaxError e: errors) {
			for (ELInstance i: instances) {
				ELInstanceImpl im = (ELInstanceImpl)i;
				if(im.contains(e.getPosition())) {
					im.addError(e);
					break;
				}
			}
		}
		
	}

	public List<SyntaxError> getSyntaxErrors() {
		return errors;
	}

	public void shift(int delta) {
		this.delta = delta;
		if(instances.size() > 0) {
			instances.get(0).getFirstToken().shift(delta);
		}
	}

}
