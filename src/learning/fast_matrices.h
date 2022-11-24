#include<vector>
#include<algorithm>
/*
 * This file implements a very simple matrix template class.
 * It supports two modes of operation:
 * 	a) With certain columns set to be zero. These columns should not be written to and do not consume any space.
 * 	b) With no columns set to zero. This class then allocates enough memory for the entire matrix, using a 
 * 		row-major representation, which should make the rescorla-wagner model run faster then.
 */


using std::vector;
using std::find_if_not;

template<class element_type>
class myMatrix
{
	public:	
		myMatrix(const myMatrix &) = delete;

		/*columns_that_are_zero */
		myMatrix(const size_t rows,const size_t cols,const vector<int> &columns_that_are_zero) : something(0)
		{
			if(columns_that_are_zero.size() != cols)
			{
				throw "Invalid columns_that_are_zero given";
			}
			zero_cols = false;
			for(const auto &c : columns_that_are_zero)
			{
				if(c == 1)
					zero_cols = true;
			}
			if(zero_cols)
			{
				this->redirection_table = vector<ssize_t>(cols,-1);
				size_t num_actual_cols = 0;
				for(size_t j = 0; j < columns_that_are_zero.size(); j++)
				{
					if(!columns_that_are_zero[j])
						redirection_table[j] = num_actual_cols++;
				}
				this->rows_internal = rows;
				this->cols_internal = cols;
				//The () at the end value-initializes everything to zero.
				internal_buffer = new element_type[rows * num_actual_cols]();
			}else
			{
				this->rows_internal = rows;
				this->cols_internal = cols;
				//The () at the end value-initializes everything to zero.
				internal_buffer  = new element_type[rows*cols]();
			}
		};

		myMatrix(myMatrix && old) : something(0)
		{
			delete[] this->internal_buffer;
			this->internal_buffer = old.internal_buffer;
			this->rows_internal = old.rows();
			this->cols_internal = old.cols();
			this->zero_cols = old.zero_cols;

			if(this->zero_cols)
			{
				this->redirection_table = old.redirection_table;
			}

			old.internal_buffer = nullptr;
			old.rows_internal = 0;
			old.cols_internal = 0;
		};
		~myMatrix()
		{
			delete[] internal_buffer;
		};
		myMatrix& operator=(const myMatrix &old)
		{
			delete[] this->internal_buffer;

			this->rows_internal = old.rows();
			this->cols_internal = old.cols();
			this->zero_cols = old.zero_cols;
			if(this->zero_cols)
			{
				this->internal_buffer = new element_type[old.rows()*old.num_actual_cols()];
				this->redirection_table = old.redirection_table;
				for(size_t j = 0; j < old.cols(); j++)
				{
					if(this->redirection_table[j] == -1)
						continue;
					for(size_t i = 0; i < old.rows(); i++)
					{
						(*this)(i,j) = old(i,j);
					}
				}
			}else
			{
				this->internal_buffer = new element_type[old.rows() * old.cols()];
				for(size_t i = 0; i < old.rows(); i++)
				{
					for(size_t j = 0; j < old.cols(); j++)
					{
						(*this)(i,j) = old(i,j);
					}
				}
			}
			return (*this);
		};
		
		myMatrix& operator=(myMatrix &&old)
		{
			delete[] this->internal_buffer;
			this->internal_buffer = old.internal_buffer;
			this->rows_internal = old.rows();
			this->cols_internal = old.cols();
			this->zero_cols = old.zero_cols;
			if(this->zero_cols)
				this->redirection_table = old.redirection_table;

			old.internal_buffer = nullptr;
			old.rows_internal = 0;
			old.cols_internal = 0;
			return (*this);
		}
		bool operator==(const myMatrix &other) const
		{
			if(this->rows() != other.rows())
				return false;
			if(this->cols() != other.cols())
				return false;
			for(size_t i = 0; i < this->rows(); i++)
			{
				for(size_t j = 0; j < this->cols(); j++)
				{
					if(this->zero_cols && this->redirection_table[j] == -1)
					{
						if(other.zero_cols && other.redirection_table[j] == -1)
							continue;
					}
					if((*this)(i,j) != other(i,j))
						return false;
				}
			}
			return true;
		}
		bool operator!=(const myMatrix &other) const
		{
			return !((*this) == other);
		}
		element_type &operator()(const size_t i,const size_t j)
		{
			if(this->zero_cols)
			{
				ssize_t actual_column = redirection_table[j];
				if(actual_column < 0)
				{
					//Doing this is undefined, we return 0 just in case someone does it anyways:
					something = element_type{0};
					return something;
				}
					return internal_buffer[i+ actual_column*this->rows()];
			}else
			{
				return internal_buffer[i*this->cols() + j];
			}
		};
		const element_type &operator()(const size_t i, const size_t j) const
		{
			if(this->zero_cols)
			{
				ssize_t actual_column = redirection_table[j];
				if(actual_column < 0)
				{
					//Doing this is undefined, but we return something.
					return something;
				}
					return internal_buffer[i +actual_column*this->rows()];
			}else
			{
				return internal_buffer[i*this->cols() + j];
			}
		};

		size_t cols() const
		{
			return cols_internal;
		};
		size_t rows() const
		{
			return rows_internal;
		};

		size_t num_actual_cols() const
		{
			if(this->zero_cols)
				return redirection_table.size();
			else
				return this->cols();
		}
		bool getZeroCols() const
		{
			return zero_cols;
		}
	private:
		bool zero_cols;
		element_type *internal_buffer = nullptr;
		size_t rows_internal;
		size_t cols_internal;
		vector<ssize_t> redirection_table;
		element_type something;//We return a reference to this if we want to return a reference to a zero value
};

