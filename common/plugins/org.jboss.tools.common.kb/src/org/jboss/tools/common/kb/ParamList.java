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
package org.jboss.tools.common.kb;

import java.util.ArrayList;
import java.util.List;

/**
 * Describes list of parametrs from Schema.
 * @author Igels
 */
public class ParamList {

	private List<Param> params = new ArrayList<Param>();

	/**
	 * 
	 * @return
	 */
	public Param[] getParams() {
		return (Param[])params.toArray(new Param[params.size()]);
	}

	/**
	 * 
	 * @param i
	 * @return
	 */
	public Param getParam(int i) {
		return (Param)params.get(i);
	}

	/**
	 * 
	 * @return
	 */
	public int getSize() {
		return params.size();
	}

	/**
	 * 
	 * @param name
	 * @param value
	 */
	public void addParam(String name, String value) {
		params.add(new Param(name, value));
	}

	/**
	 * Returns param values. This param is taken from knowledgebase schema. For example:
	 *     <AttributeType ...>
	 * 		<proposal ...>
     *			<param name="returnType" value="java.lang.String"/>
     *		</proposal>
	 *     </AttributeType>
	 * @param paramName
	 * @return
	 */
	public String[] getParamsValues(String paramName) {
		ArrayList<String> result = new ArrayList<String>();
		for(int i=0; i<params.size(); i++) {
			Param param = (Param)params.get(i);
			if(paramName.equals(param.getName())) {
				result.add(param.getValue());
			}
		}
		return result.toArray(new String[result.size()]);
	}

	/**
	 * 
	 * @author Igels
	 */
	public static class Param {

		private String name;
		private String value;

		public Param(String name, String value) {
			if(name==null || name.length()==0) {
				this.name = value;
			} else {
				this.name = name;
			}
			this.value = value;
		}

		public String getName() {
			return name;
		}

		public String getValue() {
			return value;
		}
	}
}