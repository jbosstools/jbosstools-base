package org.jboss.tools.common.java;

import java.util.List;

import org.eclipse.jdt.core.IType;

/**
 * Common interface for an annotation interface.
 * 
 * @author Viacheslav Kabanovich
 * 
 */
public interface IAnnotationType {

	/**
	 * Returns the corresponding IType of the annotation type.
	 * 
	 * @return the corresponding IType
	 */
	IType getSourceType();

	/**
	 * Returns all the available annotations which are declared for this
	 * interface.
	 * 
	 * @return all the available annotations which are declared for this
	 *         interface
	 */
	List<IAnnotationDeclaration> getAnnotationDeclarations();

	/**
	 * Returns the annotations with given type name.
	 * 
	 * @param typeName
	 * @return the annotations with given type name
	 */
	IAnnotationDeclaration getAnnotationDeclaration(String typeName);

}