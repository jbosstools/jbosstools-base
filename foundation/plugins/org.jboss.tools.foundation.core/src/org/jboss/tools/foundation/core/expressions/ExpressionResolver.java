/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.expressions;

import java.io.File;
import java.util.Map;
import java.util.Properties;

/**
 * A resolver for expressions.
 * 
 * This class requires a IVariableResolver to be in charge of 
 * resolving variables. Clients may choose to use a variety 
 * of backing datastores from which they can resolve their variables,
 * such as System Properties, a Properties object, a Map, or a private
 * model such as the eclipse variables plugin.
 * 
 * Some IVariableResolver may choose to treat the argument as a 
 * default value, while others may choose to treat it as an important
 * parameter for resolving the variable, depending on their specific 
 * usecases. 
 *
 * Syntax:
 *   Given an IVariableResolver who's backing store will return "bar" for "foo"
 *   and treats the argument as a default: 
 *      $(empty) => ExpressionResolutionException
 *      ${foo} => bar
 *      ${empty:argument} => argument
 *      ${empty,foo:argument} => bar
 *  
 *   Given an IVariableResolver who's backing store treats the argument 
 *   as a critical part of the query, look at these examples of a 
 *   4-wheel, 5-seat vehicle variable:
 *    
 *      ${numWheels} => ExpressionResolutionException  // argument is required but not provided
 *      ${numWheels:myCar} => 4  // correct usage
 *      ${numSeats, numWheels:myCar} => 4   // comma syntax leads to inappropriate result
 *      
 *   The last example may seem counter-intuitive. Some may feel
 *   that the myCar argument should distributed over both 
 *   the numSeats and the numWheels variables.
 *    
 *   It will not be. The argument will ONLY be used for the resolution
 *   of the FINAL variable in the string. In the above example, 
 *   numSeats failed to resolve, because it did not have an associated argument.  
 *   
 *   Be aware of this restriction when using the argument as a critical part of the query!
 *   Clients are in charge of ensuring that the syntax used in their strings fit their usecase. 
 *   Clients who require the argument to be treated as a critical component of the variable
 *   resolution should not allow the use of comma syntax, and should instead use only a subset 
 *   of the features supported by this ExpressionResolver. 
 *      
 *  
 * Originally based on ValueExpressionResolver from jboss-dmr.
 * 
 * This class has been modified substantially to fit it into the 
 * eclipse workflow for variable resolution
 * 
 * The state DEFAULT has been renamed to ARGUMENT for clarity and consistancy. 
 * 
 * @author <a href="mailto:david.lloyd@redhat.com">David M. Lloyd</a>
 * @author <a href="mailto:rob.stryker@redhat.com">Rob Stryker</a>
 * @since 1.1
 */
public class ExpressionResolver {

	private static final int INITIAL = 0;
	private static final int GOT_DOLLAR = 1;
	private static final int GOT_OPEN_BRACE = 2;
	private static final int RESOLVED = 3;
	private static final int ARGUMENT = 4;

	/**
	 * The private instance of the resolver we'll be using to resolve variables
	 */
	private IVariableResolver resolver;

	/**
	 * Construct a new instance using system properties 
	 * and environment variable maps to resolve variables.
	 */
	public ExpressionResolver() {
		this(new SystemPropertiesVariableResolver());
	}

	/**
	 * Construct a new instance using a map as the backing
	 * variable resolver. If the map's value objects
	 * are not String objects, the value of the toString
	 * method is what will be used. 
	 */
	public ExpressionResolver(Map<String, ? extends Object> map) {
		this(new MapVariableResolver(map));
	}

	/**
	 * Construct a new instance using a Properties object as the backing
	 * variable resolver. If the Properties value objects
	 * are not String objects, the value of the toString
	 * method is what will be used. 
	 */
	public ExpressionResolver(Properties props) {
		this(new PropertiesVariableResolver(props));
	}



	/**
	 * Construct a new instance with an arbitrary {@link IVariableResolver}
	 * 
	 *  @param resolver A variable resolver
	 */
	public ExpressionResolver(IVariableResolver resolver) {
		this.resolver = resolver;
	}


	/**
	 * Perform expression resolution.
	 * In the event of any errors, simply return the original string instead. 
	 *
	 * @param expression the expression to resolve
	 * @return the resolved string, or the original string if there were errors resolving
	 */
	public String resolveIgnoreErrors(final String value) {
		try {
			return resolve(value);
		} catch(ExpressionResolutionException ise) {
			return value; // Just return the string unchanged
		}
	}
	
	private void unusedPrivateMethod() {
		
	}

	/**
	 * Perform expression resolution.
	 *
	 * @param expression the expression to resolve
	 * @return the resolved string
	 */
	public String resolve(final String value) throws ExpressionResolutionException {
		// useless comment
		// if(value==null) return null;
		// final StringBuilder builder = new StringBuilder();
		if(value==null) return null;
		final StringBuilder builder = new StringBuilder();
		final int len = value.length();
		int state = INITIAL;
		int start = -1;
		int nest = 0;
		int nameStart = -1;
		int nameEnd = -1;
		String resolvedValue = null;
		for (int i = 0; i < len; i = value.offsetByCodePoints(i, 1)) {
			final int ch = value.codePointAt(i);
			switch (state) {
			case INITIAL: {
				switch (ch) {
				case '$': {
					state = GOT_DOLLAR;
					continue;
				}
				default: {
					builder.appendCodePoint(ch);
					continue;
				}
				}
				// not reachable
			}
			case GOT_DOLLAR: {
				switch (ch) {
				case '$': {
					builder.appendCodePoint(ch);
					state = INITIAL;
					continue;
				}
				case '{': {
					start = i + 1;
					nameStart = start;
					nameEnd = start;
					state = GOT_OPEN_BRACE;
					continue;
				}
				default: {
					// invalid; emit and resume
					builder.append('$').appendCodePoint(ch);
					state = INITIAL;
					continue;
				}
				}
				// not reachable
			}
			case GOT_OPEN_BRACE: {
				switch (ch) {
				case '{': {
					nest++;
					continue;
				}
				case ':':
				case '}':
				case ',': {
					if (nest > 0) {
						if (ch == '}') nest--;
						continue;
					}
					if (ch == ',') {
						// The next char is a comma. This variable should
						// attempt to be resolved with no argument
						final String val2 = resolveVariable(value.substring(nameStart, i).trim(), null);
						if (val2 != null) {
							builder.append(val2);
							resolvedValue = val2;
							state = ch == '}' ? INITIAL : RESOLVED;
							continue;
					}                                
						// Resolution has failed, but, we can move on to the next variable after the comma
						nameStart = i + 1;
						continue;
					} else if (ch == ':') {
						// We are now looking to harvest the argument
						nameEnd = i;
						start = i + 1;
						state = ARGUMENT;
						continue;
					} else {
						final String val2 = resolveVariable(value.substring(nameStart, i).trim(), null);
						if (val2 != null) {
							builder.append(val2);
							resolvedValue = val2;
							state = ch == '}' ? INITIAL : RESOLVED;
							continue;
					}
					throw new ExpressionResolutionException("Failed to resolve expression: "+ value.substring(start - 2, i + 1));
					}
				}
				default: {
					continue;
				}
				}
				// not reachable
			}
			case RESOLVED: {
				if (ch == '{') {
					nest ++;
				} else if (ch == '}') {
					if (nest > 0) {
						nest--;
					} else {
						state = INITIAL;
					}
				}
				continue;
			}
			case ARGUMENT: {
				if (ch == '{') {
					nest ++;
				} else if (ch == '}') {
					if (nest > 0) {
						nest --;
					} else {
						state = INITIAL;
						String s1 = value.substring(nameStart, nameEnd);
						String s2 = value.substring(start, i);
						final String val2 = resolveVariable(s1.trim(), s2);
						if (val2 != null) {
							builder.append(val2);
							resolvedValue = val2;
							state = ch == '}' ? INITIAL : RESOLVED;
							continue;
					} else {
						throw new ExpressionResolutionException("Failed to resolve expression: "+ s1 + " with argument: " + s2);
					}
				}
				}
				continue;
			}
			default:
				throw new ExpressionResolutionException("Unexpected char seen: "+ch);
			}
		}
		switch (state) {
		case GOT_DOLLAR: {
			builder.append('$');
			break;
		}
		case ARGUMENT: {
			builder.append(value.substring(start - 2));
			break;
		}
		case GOT_OPEN_BRACE: {
			// We had a reference that was not resolved, throw ISE
			if (resolvedValue == null)
				throw new ExpressionResolutionException("Incomplete expression: "+builder.toString());
			break;
		}
		}
		return builder.toString();
	}

	private String resolveVariable(String name, String argument) {
		return resolver.resolve(name, argument);
	}

	/**
	 *  This variable resolver will check a given map
	 *  to discover the value of a variable. 
	 *  
	 *  This resolver will treat the argument as a default value. 
	 */
	public static class MapVariableResolver implements IVariableResolver {
		private Map<String, ? extends Object> map;
		public MapVariableResolver(Map<String, ? extends Object> map) {
			this.map = map;
		}

		@Override
		public String resolve(String variable, String argument) {
			Object ret = map.get(variable);
			return ret == null ? argument : ret.toString();
		}
	}    

	/**
	 *  This variable resolver will check a given properties object
	 *  to discover the value of a variable. 
	 *  
	 *  This resolver will treat the argument as a default value. 
	 */
	public static class PropertiesVariableResolver implements IVariableResolver {
		private Properties props;
		public PropertiesVariableResolver(Properties props) {
			this.props = props;
		}

		@Override
		public String resolve(String variable, String argument) {
			String ret = props.getProperty(variable);
			return ret == null ? argument : ret;
		}
	}    


	/**
	 *  This variable resolver will check system properties
	 *  and environment variables to discover the value of a variable 
	 *  
	 *  This resolver will treat the argument as a default value. 
	 */
	public static class SystemPropertiesVariableResolver implements IVariableResolver {
		public SystemPropertiesVariableResolver() {
		}

		@Override
		public String resolve(String variable, String argument) {
			String ret = resolvePart(variable);
			return ret == null ? argument : ret;
		}
		/*
		 * Resolve a single name in the expression.  Return {@code null} if no resolution is possible.  The default
		 * implementation (which may be delegated to) checks system properties, environment variables, and a small set of
		 * special strings.
		 *
		 * @param name the name to resolve
		 * @return the resolved value, or {@code null} for none
		 */
		private String resolvePart(String name) {
			if ("/".equals(name)) {
				return File.separator;
			} else if (":".equals(name)) {
				return File.pathSeparator;
			}
			// First check for a key in the provided properties, otherwise env vars.
			String val = System.getProperty(name);
			if (val == null && name.startsWith("env."))
				val = System.getenv(name.substring(4));

			return val;
		}
	}
}
