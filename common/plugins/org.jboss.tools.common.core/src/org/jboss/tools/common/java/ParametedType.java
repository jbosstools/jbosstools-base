/******************************************************************************* 
 * Copyright (c) 2009,2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.java;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeParameter;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.java.TypeDeclaration.Lazy;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
//@SuppressWarnings("nls")
public class ParametedType implements IParametedType {
	protected ParametedTypeFactory typeFactory = null;
	protected IType type;
	protected int arrayIndex = 0;
	protected String signature;
	protected List<ParametedType> parameterTypes = new ArrayList<ParametedType>(1);
	protected boolean primitive;

	protected boolean isUpper = false;
	protected boolean isLower = false;
	protected boolean isVariable = false;

	boolean inheritanceIsBuilt = false;
	protected ParametedType superType = null;
	protected Collection<IParametedType> inheritedTypes = new ArrayList<IParametedType>(1);
	Set<IParametedType> allInheritedTypes = null;

	protected long inheritanceHashcode = -1;

	public static interface PositionProvider {
		ISourceRange getRange(String superTypeName);
		boolean isLoaded();
	}

	PositionProvider provider = null;

	public ParametedType() {}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.java.IParametedType#isPrimitive()
	 */
	public boolean isPrimitive() {
		return primitive;
	}

	public void setPrimitive(boolean primitive) {
		this.primitive = primitive;
	}

	public boolean isUpper() {
		return isUpper;
	}

	public void setUpper(boolean b) {
		isUpper = b;
	}

	public boolean isLower() {
		return isLower;
	}

	public void setLower(boolean b) {
		isLower = b;
	}

	public boolean isVariable() {
		return isVariable;
	}

	public void setVariable(boolean b) {
		isVariable = b;
	}

	public ParametedTypeFactory getFactory() {
		return typeFactory;
	}

	public void setFactory(ParametedTypeFactory typefactory) {
		this.typeFactory = typefactory;
	}

	public IType getType() {
		return type;
	}

	public int getArrayIndex() {
		return arrayIndex;
	}

	public String getArrayPrefix() {
		return toArrayPrefix(arrayIndex);
	}

	static String[] PREFIXES = new String[4];
	static {
		PREFIXES[0] = ""; //$NON-NLS-1$
		for (int i = 1; i < PREFIXES.length; i++) PREFIXES[i] = PREFIXES[i - 1] + Signature.C_ARRAY;
	}

	private static String toArrayPrefix(int arrayIndex) {
		return arrayIndex < PREFIXES.length ? PREFIXES[arrayIndex] : PREFIXES[3] + toArrayPrefix(arrayIndex - 3);
	}

	public String getSignature() {
		return signature;
	}

	public void setType(IType type) {
		this.type = type;
	}

	public void setSignature(String signature) {
		this.signature = signature;
		arrayIndex = 0;
		if(signature != null) {
			for (; arrayIndex < signature.length() && (signature.charAt(arrayIndex) == Signature.C_ARRAY); arrayIndex++) {}
		}
	}

	public void addParameter(ParametedType p) {
		parameterTypes.add(p);
	}

	public List<? extends IParametedType> getParameters() {
		return parameterTypes;
	}

	public void setPositionProvider(PositionProvider p) {
		provider = p;
	}

	@Override
	public boolean equals(Object object) {
		if(!(object instanceof ParametedType)) return false;
		ParametedType other = (ParametedType)object;
		return equals(other, null, iterationsLimit);
	}

	boolean equals(ParametedType other, Set<String> underConsideration,int depth) {
		if(signature != null && signature.equals(other.signature)) {
			return true;
		}
		if(type == null || other.type == null || !type.getFullyQualifiedName().equals(other.type.getFullyQualifiedName())) {
			return false;
		}
		if(parameterTypes.size() != other.parameterTypes.size()) {
			return false;
		}
		if(arrayIndex != other.arrayIndex) {
			return false;
		}
		String code = null;
		if(!parameterTypes.isEmpty()) {
			if(depth <= 0) {
				if(!equalsFailReported) {
					reportFail("ParametedType.equals()", other, underConsideration);
					equalsFailReported = true;
				}
				return false;
			}
			code = "" + getSignature() + "<-" + other.getSignature();
			if(underConsideration == null) {
				underConsideration = new HashSet<String>();
			} else if(underConsideration.contains(code)) {
				//Cycle is prevented, this case is already considered up stack.
				return true;
			}
			underConsideration.add(code);
			for (int i = 0; i < parameterTypes.size(); i++) {
				ParametedType ithis = parameterTypes.get(i);
				ParametedType iother = other.parameterTypes.get(i);
				if(!ithis.equals(iother, underConsideration, depth - 1)) {
					return false;
				}
			}
			underConsideration.remove(code);
		}

		return true;
	}

	void buildInheritance() {
		if(type == null) return;
		inheritanceHashcode = 0;
		Collection<IParametedType> inheritedTypes = new ArrayList<IParametedType>(2);
		try {
			if(!type.isInterface() && !type.isAnnotation()) {
				String sc = type.getSuperclassTypeSignature();
				boolean objectArray = false;
				if(sc != null) {
					sc = resolveParameters(sc);
				} else if(!"java.lang.Object".equals(type.getFullyQualifiedName())) { //$NON-NLS-1$
					sc = ParametedTypeFactory.OBJECT;
				} else if("java.lang.Object".equals(type.getFullyQualifiedName()) && arrayIndex > 0) { //$NON-NLS-1$
					objectArray = true;
					sc = ParametedTypeFactory.OBJECT;
				}
				if(!objectArray && arrayIndex > 0) {
					sc = getArrayPrefix() + sc;
				}
				
				superType = getFactory().getParametedType(type, this, sc);
				if(superType != null) {
					inheritanceHashcode = superType.getType().getFullyQualifiedName().hashCode();
					if(provider != null) {
						final String scn = type.getSuperclassName();
						if(scn != null) {
							if(provider.isLoaded() && provider.getRange(scn) != null) {
								ISourceRange r = provider.getRange(scn);
								superType = new TypeDeclaration(superType, type.getResource(), r.getOffset(), r.getLength());
							} else if(!provider.isLoaded()) {
								Lazy lazy = new Lazy() {
									@Override
									public void init(TypeDeclaration d) {
										ISourceRange r = provider.getRange(scn);
										if(r != null) {
											d.init(r.getOffset(), r.getLength());
										}
									}
								};
								superType = new TypeDeclaration(superType, type.getResource(), lazy);
							}

						}						
					}
					inheritedTypes.add(superType);
				}
			}
			String[] is = type.getSuperInterfaceTypeSignatures();
			if(is != null) for (int i = 0; i < is.length; i++) {
				String p = resolveParameters(is[i]);
				if(arrayIndex > 0) p = getArrayPrefix() + p;
				ParametedType t = getFactory().getParametedType(type, this, p);
				if(t != null) {
					inheritanceHashcode = inheritanceHashcode * 773 + t.getType().getFullyQualifiedName().hashCode();
					if(provider != null) {
						final String scn = type.getSuperInterfaceNames()[i];
						if(scn != null) {
							if(provider.isLoaded() && provider.getRange(scn) != null) {
								ISourceRange r = provider.getRange(scn);
								t = new TypeDeclaration(t, type.getResource(), r.getOffset(), r.getLength());
							} else if(!provider.isLoaded()) {
								Lazy lazy = new Lazy() {
									@Override
									public void init(TypeDeclaration d) {
										ISourceRange r = provider.getRange(scn);
										if(r != null) {
											d.init(r.getOffset(), r.getLength());
										}
									}
								};
								t = new TypeDeclaration(t, type.getResource(), lazy);
							}
						}
						
					}
					inheritedTypes.add(t);
				}
			}
			if(type.getCompilationUnit() != null) {
				for (IImportDeclaration d: type.getCompilationUnit().getImports()) {
					String n = d.getElementName();
					inheritanceHashcode = inheritanceHashcode * 773 + n.hashCode();
				}
			}
		} catch (JavaModelException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
		this.inheritedTypes = inheritedTypes;
		inheritanceIsBuilt = true;
	}

	public ParametedType getSuperType() {
		if(!inheritanceIsBuilt) {
			buildInheritance();
		}
		return superType;
	}

	public Collection<IParametedType> getInheritedTypes() {
		if(!inheritanceIsBuilt) {
			buildInheritance();
		}
		return inheritedTypes;
	}

	public String resolveParameters(String typeSignature) {
		if(typeSignature == null) {
			return typeSignature;
		}
		int i = typeSignature.indexOf(Signature.C_GENERIC_START);
		if(i < 0) {
			char c = typeSignature.length() == 0 ? '\0' : typeSignature.charAt(0);
			char e = typeSignature.length() == 0 ? '\0' : typeSignature.charAt(typeSignature.length() - 1);
			if((c == Signature.C_TYPE_VARIABLE || c == Signature.C_UNRESOLVED || c == Signature.C_RESOLVED) && e == Signature.C_SEMICOLON) {
				String param = typeSignature.substring(1, typeSignature.length() - 1);
				String s = findParameterSignature(param);
				return s == null ? typeSignature : s;
			}
			return typeSignature;
		}
		int j = typeSignature.lastIndexOf(Signature.C_GENERIC_END);
		if(j < i) {
			return typeSignature;
		}
		boolean replaced = false;
		StringBuffer newParams = new StringBuffer();
		String[] ps = Signature.getTypeArguments(typeSignature);
		for (String param: ps) {
			String newParam = resolveParameters( param);
			if(!param.equals(newParam)) replaced = true;
			newParams.append(newParam);
		}
		if(replaced) {
			typeSignature = typeSignature.substring(0, i) 
					+ Signature.C_GENERIC_START 
					+ newParams.toString() 
					+ Signature.C_GENERIC_END 
					+ Signature.C_SEMICOLON;
		}
		return typeSignature;
	}

	public String findParameterSignature(String paramName) {
		buildParameters();
		return signaturesByName.get(paramName);
	}

	Map<String, String> signaturesByName = null;
	Map<String, ParametedType> parametersBySignature = null;
	
	void buildParameters() {
		if(signaturesByName == null && type != null) {
			Map<String, String> sbn = new HashMap<String, String>();
			Map<String, ParametedType> pbs = new HashMap<String, ParametedType>();
			ITypeParameter[] ps = null;
			try {
				ps = type.getTypeParameters();
			} catch (JavaModelException e) {
				return;
			}
			if(ps != null) for (int i = 0; i < ps.length; i++) {
				String paramName = ps[i].getElementName();
				if(parameterTypes.size() > i) {
					ParametedType p = parameterTypes.get(i);
					sbn.put(paramName, p.getSignature());
					pbs.put(p.getSignature(), p);
				}
			}
			signaturesByName = sbn;
			parametersBySignature = pbs;
		}
	}

	public ParametedType findParameter(String signature) {
		buildParameters();
		return parametersBySignature.get(signature);
	}

	public Collection<IParametedType> getAllTypes() {
		if(allInheritedTypes == null) {
			allInheritedTypes = buildAllTypes(new HashSet<String>(), this, new HashSet<IParametedType>());
		}
		return allInheritedTypes;
	}

	Set<IParametedType> buildAllTypes(Set<String> processed, ParametedType p, Set<IParametedType> types) {
		IType t = p.getType();
		if(t != null) {
			String key = p.getArrayPrefix() + t.getFullyQualifiedName();
			if(!processed.contains(key)) {
				processed.add(key);
				types.add(p);
				Collection<IParametedType> ts = p.getInheritedTypes();
				if(ts != null) for (IParametedType pp: ts) {
					buildAllTypes(processed, (ParametedType)pp, types);
				}
			}
		}
		return types;
	}

	@Override
	public String toString() {
		return signature + ":" + super.toString(); //$NON-NLS-1$
	}

	public boolean isAssignableTo(ParametedType other, boolean checkInheritance) {
		return isAssignableTo(other, checkInheritance, new HashMap<String, IType>(), null, iterationsLimit);
	}

	private boolean isAssignableTo(ParametedType other, boolean checkInheritance, Map<String,IType> resolvedVars, Set<String> underConsideration, int depth) {
		if(equals(other)) return true;
		if("*".equals(other.getSignature())) { //$NON-NLS-1$
			return true;
		}
		if(this.type == null) {
			return (isVariable && other.isVariable && other.type == null);
		}
		if(other.isVariable && other.type == null) {
			if(type != null) {
				if(resolvedVars.get(other.getSignature()) != null) {
					return resolvedVars.get(other.getSignature()) == type;
				} else {
					resolvedVars.put(other.getSignature(), type);
				}
			}
			return true;
		}
		String code = (!parameterTypes.isEmpty() || !other.parameterTypes.isEmpty()) ? "" + getSignature() + "<-" + other.getSignature() : null;
		if(underConsideration != null && underConsideration.contains(code)) {
			//Cycle is prevented, this case is already considered up stack.
			return true;
		}
		if(this.type.equals(other.type)) {
			if(code != null) {
				if(underConsideration == null) {
					underConsideration = new HashSet<String>();
				}
				underConsideration.add(code);
			}
			if(areTypeParametersAssignableTo(other, resolvedVars, underConsideration, depth)) {
				if(code != null) {
					underConsideration.remove(code);
				}
				return true;
			}
		}
		if(checkInheritance) {
			Collection<IParametedType> ts = getAllTypes();
			if(!ts.isEmpty() && (code != null)) {
				if(code != null) {
					if(underConsideration == null) {
						underConsideration = new HashSet<String>();
					}
					underConsideration.add(code);
				}
			}
			for (IParametedType t: ts) {
				if(t == this) {
					continue;
				}
				if(((ParametedType)t).isAssignableTo(other, false, resolvedVars, underConsideration, depth)) {
					if(code != null) {
						underConsideration.remove(code);
					}
					return true;
				}
			}
		}
		if(code != null && underConsideration != null) {
			underConsideration.remove(code);
		}
		return false;
	}
	
	private boolean areTypeParametersAssignableTo(ParametedType other, Map<String,IType> resolvedVars, Set<String> underConsideration,int depth) {
		if(other.parameterTypes.isEmpty()) return true;
		if(this.parameterTypes.isEmpty()) {
			for (ParametedType p2: other.parameterTypes) {
				if("*".equals(p2.getSignature())) continue;
				if("Ljava.lang.Object;".equals(p2.getSignature())) continue;
				if("+Ljava.lang.Object;".equals(p2.getSignature())) continue;
				if(p2.isVariable()) {
					if(!p2.isLower() && !p2.isUpper()) continue;
				}
				
				return false;
			}
			return true;
		}
		if(this.parameterTypes.size() != other.parameterTypes.size()) return false;
		if(depth <= 0) {
			if(!isAssignableFailReported) {
				reportFail("ParametedType.isAssignableTo()", other, underConsideration);
				isAssignableFailReported = true;
			}
			return false;
		}
		for (int i = 0; i < parameterTypes.size(); i++) {
			ParametedType p1 = parameterTypes.get(i);
			ParametedType p2 = other.parameterTypes.get(i);
			if(p1.isLower() || (p1.isUpper() && !p1.isVariable)) return false;
			if(p1.isVariable()) {
				if(p2.isVariable()) {
					if(p2.isAssignableTo(p1, true, resolvedVars, underConsideration, depth - 1)) continue;
				} else if(p2.isLower()) {
					if(p2.isAssignableTo(p1, true, resolvedVars, underConsideration, depth - 1)) continue;
				} else if(p2.isUpper()) {
					if(p2.isAssignableTo(p1, true, resolvedVars, underConsideration, depth - 1)) continue;
					if(p1.isAssignableTo(p2, true, resolvedVars, underConsideration, depth - 1)) continue;
				} else {
					if(p2.isAssignableTo(p1, true, resolvedVars, underConsideration, depth - 1)) continue;
				}
			} else {
				if(p2.isLower()) {
					if(p2.isAssignableTo(p1, true, resolvedVars, underConsideration, depth - 1)) continue;
				} else {
					if(p1.isAssignableTo(p2, true, resolvedVars, underConsideration, depth - 1)) continue;
				}
			}
			
			return false;
		}
		return true;
	}

	static Map<String, String> primitives = new HashMap<String, String>();
	static {
		primitives.put("Integer", "int");
		primitives.put("Short", "short");
		primitives.put("Long", "long");
		primitives.put("Character", "char");
		primitives.put("Float", "float");
		primitives.put("Double", "double");
		primitives.put("Boolean", "boolean");
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.cdi.core.IParametedType#getSimpleName()
	 */
	public String getSimpleName() {
		if(getSignature()!=null) {
			if(isPrimitive()) {
				int array = arrayIndex;
				StringBuilder result = new StringBuilder(primitives.get(Signature.getSignatureSimpleName(getSignature().substring(array))));
				if(array > 0) {
					for (int i = 0; i < array; i++) {
						result.append("[]"); //$NON-NLS-1$
					}
				}
				return result.toString();
			} else {
				return Signature.getSignatureSimpleName(getSignature());
			}
		}
		return ""; //$NON-NLS-1$
	}

	public long getInheritanceCode() {
		if(!inheritanceIsBuilt) {
			buildInheritance();
		}
		return inheritanceHashcode;
	}

	static final int iterationsLimit = 10;
	static boolean equalsFailReported = false;
	static boolean isAssignableFailReported = false;

	private void reportFail(String name, ParametedType other, Set<String> underConsideration) {
		String message = "Method " + name + " failed to complete reaching the limit of " + iterationsLimit + " iterations when processing types:";
		StringBuilder sb = new StringBuilder(message);
		sb.append("\n").append(getSignature()).append(" and ").append(other.getSignature()).append("\n");
		if(underConsideration != null && !underConsideration.isEmpty()) {
			sb.append("Previous iterations processed types:").append("\n");
			for (String s: underConsideration) {
				sb.append("\t").append(s).append("\n");
			}
		}		
		CommonCorePlugin.getPluginLog().logError(sb.toString());
	}
}